package ru.circumflex.orm

class Test extends Record {

}

object Test extends Table[Test]("public", "test") {

  val id = longColumn("id").notNull
  val name = stringColumn("name").notNull.unique

  def primaryKey = pk(id)
}

object Main extends Application {

  println(Test.dialect.createTable(Test))

}