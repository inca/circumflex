package ru.circumflex.orm

import ru.circumflex.core.Circumflex

object ORMRegistry {

  var recordClassToRelationMap:Map[Class[_], Relation[_]] = Map()

  def getRelation[R](record: Record[R]): Relation[R] =
    recordClassToRelationMap.get(record.getClass) match {
      case Some(rel: Relation[R]) => rel
      case None => {
        val relation = Class
                .forName(record.getClass.getName + "$", true, Circumflex.classLoader)
                .newInstance
                .asInstanceOf[Relation[R]]
        recordClassToRelationMap += (record.getClass -> relation)
        relation
      }
    }

}