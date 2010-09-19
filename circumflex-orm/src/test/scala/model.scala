package ru.circumflex.orm

class Country extends Record[String, Country] {

  def this(code: String, name: String) = {
    this()
    this.code := code
    this.name := name
  }

  val code = "code".VARCHAR(2).NOT_NULL
      .addSetter(_.trim)
      .addSetter(_.toLowerCase)
  val name = "name".TEXT.NOT_NULL

  def PRIMARY_KEY = code
  def relation = Country
}

object Country extends Country with Table[String, Country] {
  val code_key = CONSTAINT("code_key").UNIQUE(code)
  val name_idx = "name_idx".INDEX("code")
}

class City extends Record[Long, City] with IdentityGenerator[Long, City] {
  val id = "id".BIGINT.NOT_NULL.AUTO_INCREMENT
  val name = "name".TEXT.NOT_NULL
  val country = "country_code".VARCHAR(2).NOT_NULL.REFERENCES(Country).ON_DELETE(CASCADE)

  def PRIMARY_KEY = id
  def relation = City
}

object City extends City with Table[Long, City] {
  val city_key = UNIQUE(id, country)
  override def autorefresh_?(): Boolean = true
}