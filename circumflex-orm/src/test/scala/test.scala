package ru.circumflex.orm

import ORM._

class Country extends Record[Country] {
  val code = "code" VARCHAR(2) DEFAULT("'CH'") UNIQUE
  val name = "name" VARCHAR() NOT_NULL
}

object Country extends Table[Country]

class City extends Record[City] {
  val name = "name" VARCHAR() NOT_NULL
  val country = "country_id" REFERENCES(Country) ON_DELETE CASCADE ON_UPDATE CASCADE
}

object City extends Table[City]