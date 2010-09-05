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
  // Miscellaneous
  override def toString = name.getOrElse("Unknown")
}

object Country extends Table[Country] {
  INDEX("country_code_idx", "code") USING "btree" UNIQUE

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
  def clean = {
    Country.criteria.mkDelete.execute
    City.criteria.mkDelete.execute
    Capital.criteria.mkDelete.execute
    COMMIT
  }
  def randomData(cardinality: Int): (Long, Int) = {
    val startTime = System.currentTimeMillis
    var count = 0
    val rnd = new java.util.Random()
    val chars = "abcdefghijklmnopqrstuvwxyz"
    def randomName(size: Int = 6) = (0 until size)
        .map(i => chars.charAt(rnd.nextInt(chars.length)))
        .mkString
    for (i <- 0 to cardinality) {
      val co = new Country(randomName(2), randomName())
      co.save
      count += 1
      for (j <- 0 to rnd.nextInt(6)) {
        val ci = new City(co, randomName())
        ci.save
        count += 1
        if (j == 3) {
          new Capital(co, ci).insert()
          count += 1
        }
      }
    }
    COMMIT
    return (System.currentTimeMillis - startTime, count)
  }
}
