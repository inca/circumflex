package ru.circumflex
package orm

/*!# Criteria API

The `Criteria` class provides simplified API for querying records in neat
object-oriented notation with the ability to fetch the whole hierarchy of
records in one query via _prefetching_.

Criteria API is designed to operate specifically on `Record` instances. If
you need different projections, use `Select` instead.
*/
class Criteria[PK, R <: Record[PK, R]](val rootNode: RelationNode[PK, R])
    extends SQLable with Cloneable {

  protected var _executionTime = 0l
  def executionTime = _executionTime

  private var _counter = 0
  protected def nextCounter(): Int = {
    _counter += 1
    _counter
  }

  protected var _rootTree: RelationNode[PK, R] = rootNode
  protected var _joinTree: RelationNode[PK, R] = rootNode
  protected var _prefetchSeq: Seq[Association[_, _, _]] = Nil

  protected var _projections: Seq[RecordProjection[_, _]] = List(rootNode.*)
  protected var _restrictions: Seq[Predicate] = Nil
  protected var _orders: Seq[Order] = Nil

  // Process the `prefetchSeq` of root relation
  rootNode.relation.prefetchSeq.foreach(prefetch(_))

  protected def resetProjection(projection: Projection[_]) {
    projection match {
      case a: AtomicProjection[_] => a.AS("p_" + nextCounter)
      case c: CompositeProjection[_] => c.subProjections.foreach(p => resetProjection(p))
    }
  }

  protected def replaceLeft(join: JoinNode[PK, R, _, _],
                            node: RelationNode[PK, R]): RelationNode[PK, R] =
    join.left match {
      case j: JoinNode[PK, R, _, _] => replaceLeft(j, node)
      case r: RelationNode[PK, R] => join.replaceLeft(node)
    }

  protected def updateRootTree[PKN, N <: Record[PKN, N]](
      node: RelationNode[PKN, N], association: Association[_, _, _]): RelationNode[PKN, N] =
    node match {
    // we don't actually care about types here, since type annotations are eliminated by erasure
      case j: JoinNode[PKN, N, PKN, N] =>
        j.replaceLeft(updateRootTree(j.left, association))
            .replaceRight(updateRootTree(j.right, association))
      case node: RelationNode[PKN, N] =>
        val a = association.asInstanceOf[Association[PKN, N, N]]
        if (node.relation == a.field.record.relation) {   // N == C
          new ManyToOneJoin[PKN, N, PKN, N](node, preparePf(a.parentRelation, a), a, LEFT)
        } else if (node.relation == a.parentRelation) {  // N == P
          new OneToManyJoin[PKN, N, PKN, N](node, preparePf(a.field.record.relation, a), a, LEFT)
        } else node
    }

  protected def preparePf[PKN, N <: Record[PKN, N]](
      relation: Relation[PKN, N], association: Association[_, _, _]): RelationNode[PKN, N] = {
    val node = relation.AS("pf_" + nextCounter)
    _projections ++= List(node.*)
    _prefetchSeq ++= List[Association[_, _, _]](association)
    node
  }

  protected def updateJoinTree[PKN, N <: Record[PKN, N]](
      node: RelationNode[PKN, N], tree: RelationNode[PK, R]): RelationNode[PK, R] =
    tree match {
    // outra vez, types are not really important here
      case j: JoinNode[PK, R, PK, R] => try {   // try the left side
        j.replaceLeft(updateJoinTree(node, j.left))
        j.replaceRight(updateJoinTree(node, j.right))
      } catch {
        case e: Exception =>                    // try the right side
          j.replaceRight(updateJoinTree(node, j.right))
      }
      case rel: RelationNode[PK, R] => rel.JOIN(node)
    }

  protected def processTupleTree[PKN, N <: Record[PKN, N]](
      tuple: Array[_], tree: RelationNode[PKN, N]) {
    tree match {
      case j: OneToManyJoin[PKN, N, PKN, N] =>
        val pNode = j.left
        val cNode = j.right
        val a = j.association
        val pIndex = _projections.indexWhere(_.node.alias == pNode.alias)
        val cIndex = _projections.indexWhere(_.node.alias == cNode.alias)
        if (pIndex == -1 || cIndex == -1) return
        tuple(pIndex).asInstanceOf[Option[N]] map {
          parent =>
            var children = tx.cache.cacheInverse(parent.PRIMARY_KEY(), a, Nil)
            tuple(cIndex).asInstanceOf[Option[N]] map {
              child =>
                if (!children.contains(child))
                  children ++= List(child)
                tx.cache.updateInverse(parent.PRIMARY_KEY(), a, children)
            }
        }
        processTupleTree(tuple, j.left)
        processTupleTree(tuple, j.right)
      case j: JoinNode[_, _, _, _] =>
        processTupleTree(tuple, j.left)
        processTupleTree(tuple, j.right)
      case _ =>
    }
  }

  def add(predicates: Predicate*): Criteria[PK, R] = {
    _restrictions ++= predicates.toList
    this
  }
  def add(expression: String, params: Pair[String, Any]*): Criteria[PK, R] =
    add(prepareExpr(expression, params: _*))

  def addOrder(orders: Order*): Criteria[PK, R] = {
    _orders ++= orders.toList
    this
  }

  def prefetch(association: Association[_, _, _]): Criteria[PK, R] = {
    val a = association.asInstanceOf[Association[PK, R, R]]
    if (!_prefetchSeq.contains(a)) {
      // The depth-search is used to update query plan if possible.
      _rootTree = updateRootTree(_rootTree, a)
      // Also process `prefetchSeq` of parent and child relations
      a.parentRelation.prefetchSeq.foreach(prefetch(_))
      a.record.relation.prefetchSeq.foreach(prefetch(_))
    }
    this
  }

  def addJoin[PKN, N <: Record[PKN, N]](node: RelationNode[PKN, N]): Criteria[PK, R] = {
    _joinTree = updateJoinTree(node, _joinTree)
    this
  }

  /*!## Limits & offsets

  Use methods `limit` and `offset` to provide paging/scrolling on the result set
  of the select query produced by `mkSelect`. Note that these clauses interfere
  with one-to-many joins or prefetches, which typically require larger result sets.

  If you find yourself in the situation where you are required to use one-to-many
  prefetching or joins, simulate result set limiting using subqueries in the `WHERE`
  clause.
  */

  protected var _limit: Int = -1
  def limit: Int = _limit
  def limit(l: Int): this.type = {
    _limit = l
    this
  }

  protected var _offset: Int = 0
  def offset: Int = _offset
  def offset(o: Int): this.type = {
    _offset = o
    this
  }

  // Querying

  def mkSelect(): SQLQuery[Array[Option[Any]]] =
    SELECT(new UntypedTupleProjection(projections: _*))
        .FROM(queryPlan)
        .WHERE(predicate)
        .ORDER_BY(_orders: _*)
        .LIMIT(limit)
        .OFFSET(offset)
  def mkUpdate(): Update[PK, R] = UPDATE(rootNode).WHERE(predicate)
  def mkDelete(): Delete[PK, R] = DELETE(rootNode).WHERE(predicate)

  def projections: Seq[Projection[_]] = {
    _projections.foreach(p => resetProjection(p))
    _projections
  }

  def predicate: Predicate = _restrictions.size match {
    case 0 => EmptyPredicate
    case 1 => _restrictions(0)
    case _ => orm.AND(_restrictions: _*)
  }

  def queryPlan: RelationNode[PK, R] = _joinTree match {
    case j: JoinNode[PK, R, _, _] => replaceLeft(j.clone(), _rootTree)
    case r: RelationNode[PK, R] => _rootTree
  }

  def list(): Seq[R] = {
    val q = mkSelect()
    val result = q.resultSet { rs =>
      var result: Seq[R] = Nil
      while (rs.next) q.read(rs) map { tuple =>
        processTupleTree(tuple, _rootTree)
        val root = tuple(0).asInstanceOf[Option[R]].get
        if (!result.contains(root))
          result ++= List(root)
      }
      result
    }
    _executionTime = q.executionTime
    result
  }

  def unique(): Option[R] = {
    val q = mkSelect()
    val result = q.resultSet { rs =>
      if (!rs.next) None     // none records found
      // Okay, let's grab the first one. This would be the result eventually.
      else q.read(rs) map { firstTuple =>
        processTupleTree(firstTuple, _rootTree)
        val result = firstTuple(0).asInstanceOf[Option[R]].get
        // We don't want to screw prefetches up so let's walk till the end,
        // but make sure that no other root records appear in result set.
        while (rs.next) {
          q.read(rs) map { tuple =>
            processTupleTree(tuple, _rootTree)
            val root = tuple(0).asInstanceOf[Option[Any]].get
            if (root != result)   // Wow, this thingy shouldn't be here, call the police!
              throw new ORMException("Unique result expected, but multiple records found.")
          }
        }
        result
      }
    }
    _executionTime = q.executionTime
    result
  }

  def toSql = mkSelect().toSql

  override def toString = queryPlan.toString

  /*!## Criteria Merging

  Several `Criteria` objects can be merged using `AND` and `OR` operators.
  Merging implies following actions:

    * this criteria object is shallowly cloned prior to merging so that the
    source is not modified;
    * the root aliases of both criteria must match or `ORMException` will
    be thrown;
    * alias counters are summed to prevent collisions;
    * every association from specified `criteria` prefetch sequence is added to
    the result criteria prefetch sequence, thus updating it's query plan;
    * next, the join tree of specified `criteria` is merged with the join tree of
    the result criteria;
    * finally, restrictions and order specificators are copied from specified
    `criteria` to the result criteria, specified `operator` is applied to
    restrictions.

  Note, however, that alias collision can occur while merging criteria with
  joins. It is a best practice to assign join aliases manually.
  */
  protected def merge(criteria: Criteria[PK, R], operator: String): Criteria[PK, R] = {
    val result = this.clone.asInstanceOf[Criteria[PK, R]]
    // compare aliases
    if (result.rootNode.alias != criteria.rootNode.alias)
      throw new ORMException("Criteria root aliases must match for successful merging.")
    // ensure counter integrity
    result._counter += criteria._counter
    // add prefetches
    criteria._prefetchSeq.foreach(a => result.prefetch(a))
    // update join tree
    result._joinTree = criteria._joinTree match {
      case j: JoinNode[PK, R, _, _] => result.replaceLeft(j.clone(), result._joinTree)
      case _ => result._joinTree
    }
    // copy restrictions
    result._restrictions = List(new AggregatePredicate(
      operator, List(result.predicate, criteria.predicate)))
    // copy order specificators
    criteria._orders.foreach { o =>
      if (!result._orders.contains(o))
        result.addOrder(o)
    }
    result
  }

  def AND(criteria: Criteria[PK, R]): Criteria[PK, R] = merge(criteria, ormConf.dialect.AND)
  def OR(criteria: Criteria[PK, R]): Criteria[PK, R] = merge(criteria, ormConf.dialect.OR)

  /*! Criteria can be merged with inverse associations to create logical scopes. Same
  rules are applied as with criteria merging, except that the criteria object with
  proper restrictions is created from the inverse association implicitly.
  */
  protected def merge(inverse: InverseAssociation[_, R, _, _], operator: String): Criteria[PK, R] = {
    val criteria = new Criteria[PK, R](rootNode)
    aliasStack.push(rootNode.alias)
    criteria.add(inverse.association.asInstanceOf[Association[_, _, R]] IS inverse.record.asInstanceOf[R])
    merge(criteria, operator)
  }
  def AND(inverse: InverseAssociation[_, R, _, _]): Criteria[PK, R] = merge(inverse, ormConf.dialect.AND)
  def OR(inverse: InverseAssociation[_, R, _, _]): Criteria[PK, R] = merge(inverse, ormConf.dialect.OR)

}