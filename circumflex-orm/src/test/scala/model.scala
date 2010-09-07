package ru.circumflex.orm

import xml._

class Country extends Record[Country] {
  // Constructor shortcuts
  def this(code: String, name: String) = {
    this()
    this.code := code
    this.name := name
  }
  // Fields
  val code = "code" VARCHAR(2) DEFAULT("'ch'") SETTER(_.toLowerCase)
  val name = "name" TEXT
  // Inverse associations
  def cities = inverse(City.country)
  // Miscellaneous
  override def toString = name.getOrElse("Unknown")
}

object Country extends Table[Country] {
  UNIQUE(this.code)

  def byCode(c: String) = criteria.add(this.code EQ c.toLowerCase).unique
  def byName(n: String) = criteria.add(this.name LIKE n).list

  validation.notEmpty(_.code)
      .notEmpty(_.name)
      .pattern(_.code, "(?i:[a-z]{2})")
}

class City extends Record[City] {
  // Constructor shortcuts
  def this(country: Country, name: String) = {
    this()
    this.country := country
    this.name := name
  }
  // Fields
  val name = "name" TEXT
  // Associations
  val country = "country_id" REFERENCES(Country) ON_DELETE CASCADE ON_UPDATE CASCADE
  // Miscellaneous
  override def toString = name.getOrElse("Unknown")
}

object City extends Table[City] {
  validation.notEmpty(_.name)
      .notNull(_.country.field)
}

class Capital extends Record[Capital] {
  // Constructor shortcuts
  def this(country: Country, city: City) = {
    this()
    this.country := country
    this.city := city
  }
  // Associations
  val country = "country_id" REFERENCES(Country) ON_DELETE CASCADE
  val city = "city_id" REFERENCES(City) ON_DELETE CASCADE
}

object Capital extends Table[Capital] {
  UNIQUE (this.country)
  UNIQUE (this.city)
}