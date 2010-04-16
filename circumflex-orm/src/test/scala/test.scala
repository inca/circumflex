package ru.circumflex.orm

class Country extends Relation[Country] {
  val name = stringColumn := "Switzerland"
}

class City extends Relation[City] {
  val name = stringColumn := "Lausanne"
}