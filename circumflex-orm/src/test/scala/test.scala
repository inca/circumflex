package ru.circumflex.orm

import ORM._

class Country extends Record[Country] {
  val code = "code" VARCHAR(2) DEFAULT("'ch'")
  val name = "name" TEXT
}

object Country extends Table[Country] {
  INDEX("country_code_idx", "LOWER(code)") USING "btree"
}

class City extends Record[City] {
  val name = "name" TEXT
  val country = "country_id" REFERENCES(Country) ON_DELETE CASCADE ON_UPDATE CASCADE
}

object City extends Table[City] {
  CONSTRAINT ("city_name_country_id_key") UNIQUE (this.name, this.country)
}
