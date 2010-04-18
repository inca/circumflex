package ru.circumflex.orm

class Country extends Record[Country] {
  val name = text := "Switzerland"
}

class City extends Record[City] {
  val name = text := "Lausanne"
  val beautiful = boolean := true
}