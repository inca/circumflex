package ru.circumflex.orm

class Query(override val configuration: Configuration)
    extends Configurable {

  var projections: Seq[Projection[_]] = Nil

  var relations: Seq[RelationNode[_]] = Nil

  def toSql = configuration.dialect.select(this)
  
}