package ru.circumflex.orm

class Country extends Record[String, Country] {

  val code = new Field[String](this, "code", "varchar(2)")
      .addSetter(_.trim)
      .addSetter(_.toLowerCase)
  val name = new Field[String](this, "name", "text")

  def PRIMARY_KEY = code
  val relation = Country
}

object Country extends Country with Table[String, Country] {
  
}

class City extends Record[Long, City] {

  val id = new Field[Long](this, "id", "bigint")
  val name = new Field[String](this, "name", "text")

  def PRIMARY_KEY = id
  val relation = City

}

object City extends City with Table[Long, City] {
  
}