package ru.circumflex.orm

class Country extends Record[String, Country] {
  val code = "code".VARCHAR(2).NOT_NULL
      .addSetter(_.trim)
      .addSetter(_.toLowerCase)
  val name = "name".TEXT.NOT_NULL

  def PRIMARY_KEY = code
  val relation = Country
}

object Country extends Country with Table[String, Country] {
  val code_key = CONSTAINT("code_key").UNIQUE(code)
  val name_idx = "name_idx".INDEX("code")
}

class City extends Record[Long, City] {
  val id = "id".BIGINT.NOT_NULL
  val name = "name".TEXT.NOT_NULL
  val country = "country_code".VARCHAR(2).REFERENCES(Country).ON_DELETE(CASCADE)

  def PRIMARY_KEY = id
  val relation = City
}

object City extends City with Table[Long, City] {
  val city_key = UNIQUE(id, country)
}