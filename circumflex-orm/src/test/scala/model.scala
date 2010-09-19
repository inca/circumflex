package ru.circumflex.orm

import ru.circumflex.core._

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
  def cities = inverseMany(City.country)

  def PRIMARY_KEY = code
  def relation = Country
}

object Country extends Country with Table[String, Country] {
  val codeKey = CONSTRAINT("code_key").UNIQUE(code)
  val nameIdx = "name_idx".INDEX("code")

  validation.notEmpty(_.code) add { r =>
    if (r.code.null_?) None
    else if (r.code().length != 2) Some(Msg("customValidation"))
    else None
  }
}

class City extends Record[Long, City] with IdentityGenerator[Long, City] {

  def this(name: String, country: Country) = {
    this()
    this.name := name
    this.country := country
  }

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