package ru.circumflex.orm

import collection.mutable.ListBuffer

// ## Script for Database Schema Export

class DDLScript {

  val schemata = new ListBuffer[String]
  val tables = new ListBuffer[Table[_]]
  val views = new ListBuffer[View[_]]
  val constraints = new ListBuffer[Constraint]
  val preAux = new ListBuffer[SchemaObject]
  val postAux = new ListBuffer[SchemaObject]

  def addSchema(schema: String) = {
    val s = schema.toLowerCase
    if (!schemata.contains(s)) schemata += s
  }

  def add(obj: SchemaObject): this.type = {
    def processRelation(r: Relation[_]) = {
      addSchema(r.schema)
      r.preAux.foreach(o =>
        if (!preAux.contains(o)) preAux += o)
      r.postAux.foreach(o => add(o))
    }
    obj match {
      case t: Table[_] =>
        tables += t
        t.constraints.foreach(o => add(o))
        processRelation(t)
      case v: View[_] =>
        views += v
        processRelation(v)
      case c: Constraint => constraints += c
      case o =>
        if (!postAux.contains(o))
          postAux += o
    }
    return this
  }

}