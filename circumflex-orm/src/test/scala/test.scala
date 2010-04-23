package ru.circumflex.orm

import ORM._

class Country extends Record[Country] {
  val code = "code" VARCHAR(2) DEFAULT("'CH'") UNIQUE
  val name = "name" TEXT
}

object Country extends Table[Country]

class City extends Record[City] {
  val name = "name" TEXT
  val country = "country_id" REFERENCES(Country) ON_DELETE CASCADE ON_UPDATE CASCADE
}

object City extends Table[City]