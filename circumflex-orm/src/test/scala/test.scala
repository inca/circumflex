package ru.circumflex.orm

import ORM._

class Country extends Record[Country] {
  val code = VARCHAR(2) DEFAULT("'CH'") UNIQUE
  val name = VARCHAR() NOT_NULL
}

object Country extends Table[Country]

class City extends Record[City] {
  val name = VARCHAR() NOT_NULL
  val country = REFERENCE_TO(Country) ON_DELETE CASCADE ON_UPDATE CASCADE
}

object City extends Table[City]