package ru.circumflex.orm

// ## Criteria API

/**
 * **Criteria API** is a simplified queriyng interface which allows
 * to fetch records in neat object-oriented notation with the ability to
 * fetch the whole hierarchy of records in one query via *prefetching*.
 *
 * Criteria API is designed to operate on `Record`s specifically. If
 * you want to use different projections, use `Select` instead.
 */
class Criteria[R <: Record[R]](val rootNode: RelationNode[R])
    extends SQLable with Cloneable {

  private var _counter = 0
  protected def nextCounter: Int = {
    _counter += 1
    _counter
  }

  protected var _rootTree: RelationNode[R] = rootNode
  protected var _joinTree: RelationNode[R] = rootNode
  protected var _prefetchSeq: Seq[Association[_, _]] = Nil

  protected var _projections: Seq[RecordProjection[_]] = List(rootNode.*)
  protected var _restrictions: Seq[Predicate] = Nil
  protected var _orders: Seq[Order] = Nil

  protected var _limit: Int = -1
  protected var _offset: Int = 0

  // ### Internal stuff

  /**
   * Renumber specified `projection` aliases and it's `subProjections` recursively
   * so that no confusions happen.
   */
  protected def resetProjection(projection: Projection[_]): Unit = projection match {
    case a: AtomicProjection[_] => a.as("p_" + nextCounter)
    case c: CompositeProjection[_] => c.subProjections.foreach(p => resetProjection(p))
  }

  /**
   * Replace left-most node of specified `join` with specified `node`.
   */
  protected def replaceLeft(join: JoinNode[R, _],
                            node: RelationNode[R]): RelationNode[R] =
    join.left match {
      case j: JoinNode[R, _] => replaceLeft(j, node)
      case r: RelationNode[R] => join.replaceLeft(node)
    }

  /**
   * Attempt to search the root tree of query plan for relations of specified `association`
   * and correspondingly update it if necessary.
   */
  protected def updateRootTree[N <: Record[N], P <: Record[P], C <: Record[C]](
      node: RelationNode[N],
      association: Association[C, P]): RelationNode[N] =
    node match {
      case j: JoinNode[C, P] => j.replaceLeft(updateRootTree(j.left, association))
          .replaceRight(updateRootTree(j.right, association))
      case j: JoinNode[P, C] => j.replaceLeft(updateRootTree(j.left, association))
          .replaceRight(updateRootTree(j.right, association))
      case node: RelationNode[N] =>
        if (node.relation == association.record.relation) {   // N == C
          val a = association.asInstanceOf[Association[N, P]]
          new ManyToOneJoin(node, preparePf(a.foreignRelation, a), a, LEFT)
        } else if (node.relation == association.foreignRelation) {  // N == P
          val a = association.asInstanceOf[Association[C, N]]
          new OneToManyJoin(node, preparePf(a.record.relation, a), a, LEFT)
        } else node
    }

  /**
   * Prepare specified `node` and `association` to participate in prefetching.
   */
  protected def preparePf[N <: Record[N]](relation: Relation[N],
                                          association: Association[_, _]): RelationNode[N] = {
    val node = relation.as("pf_" + nextCounter)
    _projections ++= List(node.*)
    _prefetchSeq ++= List[Association[_,_]](association)
    return node
  }

  /**
   * Perform a depth-search and add specified `node` to specified `tree` of joins.
   */
  protected def updateJoinTree[N <: Record[N]](node: RelationNode[N],
                                               tree: RelationNode[R]): RelationNode[R] =
    tree match {
      case j: JoinNode[R, R] => try {   // try the left side
        j.replaceLeft(updateJoinTree(node, j.left))
      } catch {
        case e: ORMException =>         // try the right side
          j.replaceRight(updateJoinTree(node, j.right))
      }
      case rel: RelationNode[R] => rel.join(node)
    }

  /**
   * Extract the information for inverse associations from specified `tuple` using
   * specified `tree`, which should appear to be a subtree of query plan.
   */
  protected def processTupleTree[N <: Record[N], P <: Record[P], C <: Record[C]](
      tuple: Array[_], tree: RelationNode[N]): Unit =
    tree match {
      case j: OneToManyJoin[P, C] =>
        val pNode = j.left
        val cNode = j.right
        val a = j.association
        val pIndex = _projections.findIndexOf(p => p.node.alias == pNode.alias)
        val cIndex = _projections.findIndexOf(p => p.node.alias == cNode.alias)
        if (pIndex == -1 || cIndex == -1) return
        val parent = tuple(pIndex).asInstanceOf[P]
        val child = tuple(cIndex).asInstanceOf[C]
        if (parent != null) {
          val children = tx.getCachedInverse(parent, a) match {
            case null => Nil
            case l: Seq[C] => l
          }
          if (child != null && !children.contains(child))
            tx.updateInverseCache(parent, a, children ++ List(child))
        }
        processTupleTree(tuple, j.left)
        processTupleTree(tuple, j.right)
      case j: JoinNode[_, _] =>
        processTupleTree(tuple, j.left)
        processTupleTree(tuple, j.right)
      case _ =>
    }
  
  protected def prepareLimitOffsetPredicate: Predicate = {
    val n = rootNode.clone.as("__lo")
    val q = SELECT (n.id) FROM (n) LIMIT (_limit) OFFSET (_offset) ORDER_BY (_orders: _*)
    return rootNode.id IN (q)  
  }

  // ## Public Stuff

  /**
   * Add specified `predicates` to restrictions list.
   */
  def add(predicates: Predicate*): Criteria[R] = {
    _restrictions ++= predicates.toList
    return this
  }
  def add(expression: String, params: Pair[String, Any]*): Criteria[R] =
    add(prepareExpr(expression, params: _*))

  /**
   * Add specified `orders` to order specificators list.
   */
  def addOrder(orders: Order*): Criteria[R] = {
    _orders ++= orders.toList
    return this
  }

  /**
   * Set the maximum amount of root records that will be returned by the query.
   */
  def limit(value: Int): Criteria[R] = {
    this._limit = value
    return this
  }

  /**
   * Set the offset for root records that will be returned by the query.
   */
  def offset(value: Int): Criteria[R] = {
    this._offset = value
    return this
  }

  /**
   * Add specified `association` to prefetch list.
   */
  def prefetch[P <: Record[P], C <: Record[C]](association: Association[C, P]): Criteria[R] = {
    if (!_prefetchSeq.contains(association)) {
      // The depth-search is used to update query plan if possible.
      _rootTree = updateRootTree(_rootTree, association)
      // TODO also process prefetch list of both sides of association.
    }
    return this
  }

  /**
   * Add specified `node` to join tree so that you can build queries with transitive criteria.
   */
  def addJoin[N <: Record[N]](node: RelationNode[N]): Criteria[R] = {
    _joinTree = updateJoinTree(node, _joinTree)
    return this
  }

  /**
   * Make an SQL SELECT query from this criteria.
   */
  def mkSelect: SQLQuery[Array[Any]] =
    SELECT(new UntypedTupleProjection(projections: _*))
        .FROM(queryPlan)
        .WHERE(predicate)
        .ORDER_BY(_orders: _*)

  /**
   * Make a DML `UPDATE` query from this criteria. Only `WHERE` clause is used, all the
   * other stuff is ignored.
   */
  def mkUpdate: Update[R] = UPDATE(rootNode.relation).WHERE(predicate)

  /**
   * Make a DML `DELETE` query from this criteria. Only `WHERE` clause is used, all the
   * other stuff is ignored.
   */
  def mkDelete: Delete[R] = DELETE(rootNode).WHERE(predicate)

  /**
   * Renumber the aliases of all projections so that no confusions happen.
   */
  def projections: Seq[Projection[_]] = {
    _projections.foreach(p => resetProjection(p))
    return _projections
  }

  /**
   * Compose an actual predicate from restrictions.
   */
  def predicate: Predicate = {
    if (_limit != -1 || _offset != 0)
      add(prepareLimitOffsetPredicate)
    if (_restrictions.size > 0)
      AND(_restrictions: _*)
    else EmptyPredicate
  }

  /**
   * Merge the *join tree* with *prefetch tree* to form an actual `FROM` clause.
   */
  def queryPlan: RelationNode[R] = _joinTree match {
    case j: JoinNode[R, _] => replaceLeft(j.clone, _rootTree)
    case r: RelationNode[R] => _rootTree
  }

  /**
   * Execute a query, process prefetches and retrieve the list of records.
   */
  def list: Seq[R] = {
    val q = mkSelect
    q.resultSet(rs => {
      var result: Seq[R] = Nil
      while (rs.next) {
        val tuple = q.read(rs)
        processTupleTree(tuple, _rootTree)
        val root = tuple(0).asInstanceOf[R]
        if (!result.contains(root))
          result ++= List(root)
      }
      return result
    })
  }

  /**
   * Execute a query, process prefetches and retrieve unique root record. If result set
   * yields multiple root records, an exception is thrown.
   */
  def unique: Option[R] = {
    val q = mkSelect
    q.resultSet(rs => {
      if (!rs.next) return None     // none records found
      // Okay, let's grab the first one. This would be the result eventually.
      val firstTuple = q.read(rs)
      processTupleTree(firstTuple, _rootTree)
      val result = firstTuple(0).asInstanceOf[R]
      if (result == null) return None
      // We don't want to screw prefetches up so let's walk till the end,
      // but make sure that no other root records appear in result set.
      while (rs.next) {
        val tuple = q.read(rs)
        processTupleTree(tuple, _rootTree)
        val root = tuple.apply(0)
        if (root != result)   // Wow, this thingy shouldn't be here, call the police!
          throw new ORMException("Unique result expected, but multiple records found.")
      }
      return Some(result)
    })
  }

  // ### Miscellaneous

  def toSql = mkSelect.toSql

  override def toString = queryPlan.toString

}
