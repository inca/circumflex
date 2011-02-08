package ru.circumflex.orm

class Country extends Record[String, Country] {
  def this(code: String, name: String) = {
    this()
    this.code := code
    this.name := name
  }

  val code = "code".VARCHAR(2).NOT_NULL
      .addSetter(_.trim)
      .addSetter(_.toLowerCase)
  val name = "name".TEXT.NOT_NULL
  def cities = inverseMany(City.country)
  def capital = inverseOne(Capital.country)

  def PRIMARY_KEY = code
  def relation = Country
  override def toString = name.getOrElse("<unknown>")
}

object Country extends Country
with Table[String, Country]
with Cacheable[String, Country] {
  val codeKey = CONSTRAINT("code_key").UNIQUE(code)
  val nameIdx = "name_idx".INDEX("code")

  validation.notEmpty(_.code).pattern(_.code, "^[a-z]{2}$", "syntax")
}

class City extends Record[Long, City] with IdentityGenerator[Long, City] {
  def this(name: String, country: Country) = {
    this()
    this.name := name
    this.country := country
  }

  val id = "id".BIGINT.NOT_NULL.AUTO_INCREMENT
  val name = "name".TEXT.NOT_NULL
  val country = "country_code".VARCHAR(2).NOT_NULL.REFERENCES(Country).ON_DELETE(CASCADE)

  def PRIMARY_KEY = id
  def relation = City
  override def toString = name.getOrElse("<unknown>")
}

object City extends City with Table[Long, City] with Cacheable[Long, City] {
  val cityKey = UNIQUE(name, country)
  def byName(name: String) = (City AS "ci").map(ci =>
    ci.criteria.add(ci.name LIKE name).addOrder(ci.name ASC))
}

class Capital extends Record[String, Capital] {
  def this(country: Country, city: City) = {
    this()
    this.country := country
    this.city := city
  }
  val country = "country_id".VARCHAR(2).NOT_NULL.REFERENCES(Country).ON_DELETE(CASCADE)
  val city = "city_id".BIGINT.NOT_NULL.REFERENCES(City).ON_DELETE(CASCADE)

  def relation = Capital
  def PRIMARY_KEY = country.field
  override def toString = city().name.getOrElse("<unknown>")
}

object Capital extends Capital with Table[String, Capital] with Cacheable[String, Capital] {
  val cityKey = UNIQUE(city)
}

// Pair field

class Person extends Record[(String, String), Person] {
  def relation = Person
  def PRIMARY_KEY = passport

  val passportSerial = "passport_serial".TEXT.NOT_NULL
  val passportNumber = "passport_number".TEXT.NOT_NULL
  val passport = (passportSerial -> passportNumber).REFERENCES(Passport).ON_DELETE(CASCADE)
}

object Person extends Person with Table[(String, String), Person]


class Passport extends Record[(String, String), Passport] {
  def relation = Passport
  def PRIMARY_KEY = (serial -> number)

  val serial = "serial".TEXT.NOT_NULL
  val number = "number".TEXT.NOT_NULL
}

object Passport extends Passport with Table[(String, String), Passport]
