package ru.circumflex.orm

class Test extends Record {

}

object Test extends Table[Test]("public", "test") {

  val id = bigintColumn("id").notNull
  val name = stringColumn("name").notNull.unique

  def primaryKey = pk(id)
}

object Main extends Application {

  Test.columns.foreach(col => println(col.sqlDefinition))

}