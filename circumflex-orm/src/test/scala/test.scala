package ru.circumflex.orm

class Country extends Record[Country] {
  val code = "code" VARCHAR(2) DEFAULT("'ch'")
  val name = "name" TEXT
  override def toString = name()
}

object Country extends Table[Country] {
  INDEX("country_code_idx", "LOWER(code)") USING "btree" UNIQUE
}

class City extends Record[City] {
  val name = "name" TEXT
  val country = "country_id" REFERENCES(Country) ON_DELETE CASCADE ON_UPDATE CASCADE
  override def toString = name()
}

object City extends Table[City] {
  CONSTRAINT ("city_name_country_id_key") UNIQUE (this.name, this.country)
}
