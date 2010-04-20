package ru.circumflex.orm

class Country extends Record[Country] {
  val name = TEXT.NOT_NULL.DEFAULT("'Switzerland'") := "Switzerland"
}

class City extends Record[City] {
  val name = TEXT := "Lausanne"
  val beautiful = BOOLEAN := true
}