package ru.circumflex.orm

class Person extends Record {

}

object Person extends Table[Person]("public", "person") {

  def primaryKey = pk(id)

  val id = longColumn("id").notNull
  val login = stringColumn("login").notNull.unique

}



object Main extends Application {

  println(Person.dialect.createTable(Person))

}