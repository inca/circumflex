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
  val code = "code" VARCHAR(2) DEFAULT("'ch'")
  val name = "name" TEXT
  // Inverse associations
  def cities = inverse(City.country)
  // Validations
  validation.notEmpty(code)
      .notEmpty(name)
      .pattern(code, "(?i:[a-z]{2})")
  // Miscellaneous
  override def toString = name.getOrElse("Unknown")
}

object Country extends Table[Country] {
  INDEX("country_code_idx", "LOWER(code)") USING "btree" UNIQUE
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
  // Validations
  validation.notEmpty(name)
      .notNull(country.field)
  // Miscellaneous
  override def toString = name.getOrElse("Unknown")
}

object City extends Table[City]

class Capital extends Record[Capital] {
  // Constructor shortcuts
  def this(country: Country, city: City) = {
    this()
    this.country := country
    this.city := city
  }
  // Associations
  val country = "country_id" REFERENCES(Country) ON_DELETE CASCADE
  val city = "city_id" REFERENCES(City) ON_DELETE RESTRICT
}

object Capital extends Table[Capital] {
  UNIQUE (this.country)
  UNIQUE (this.city)
}

object Sample {
  def schema = new DDLUnit(City, Capital, Country).dropCreate
      .messages.foreach(msg => println(msg.body))
  def selects = {
    val ci = City as "ci"
    val co = Country as "co"
    // Select countries with corresponding cities:
    val s1 = SELECT (co.*, ci.*) FROM (co JOIN ci) list // Seq[(Country, City)]
    // Select countries and count their cities:
    val s2 = SELECT (co.*, COUNT(ci.id)) FROM (co JOIN ci) GROUP_BY (co.*) list // Seq[(Country, Int)]
    // Select all russian cities:
    val s3 = SELECT (ci.*) FROM (ci JOIN co) WHERE (co.code LIKE "ru") ORDER_BY (ci.name ASC) list  // Seq[City]
  }
  def data = Deployment
      .readAll(XML.load(getClass.getResourceAsStream("/test.cxd.xml")))
      .foreach(_.process)
}
