package ru.circumflex.orm

class Country extends Record[Country] {
  val code = VARCHAR(2).UNIQUE.DEFAULT("'CH'") := "CH"
  val name = TEXT := "Switzerland"
}

object Country extends Table[Country]

class City extends Record[City] {
  val name = TEXT := "Lausanne"
}

object City extends Table[City]