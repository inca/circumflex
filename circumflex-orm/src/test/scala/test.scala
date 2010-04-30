package ru.circumflex.orm

class Country extends Record[Country] {
  val code = "code" VARCHAR(2) DEFAULT("'ch'")
  val name = "name" TEXT
  override def toString = name.getOrElse("Unknown")
}

object Country extends Table[Country] {
  INDEX("country_code_idx", "LOWER(code)") USING "btree" UNIQUE
}

class City extends Record[City] {
  val name = "name" TEXT
  val country = "country_id" REFERENCES(Country) ON_DELETE CASCADE ON_UPDATE CASCADE
  override def toString = name.getOrElse("Unknown")
}

object City extends Table[City]

class Capital extends Record[Capital] {
  val country = "country_id" REFERENCES(Country) ON_DELETE CASCADE
  val city = "city_id" REFERENCES(City) ON_DELETE RESTRICT
}

object Capital extends Table[Capital] {
  CONSTRAINT "capital_uniq" UNIQUE (this.country, this.city)
}
