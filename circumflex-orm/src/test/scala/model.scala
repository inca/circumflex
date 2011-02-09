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

// Many to many via with composite key

class Developer extends Record[String, Developer] {
  def relation = Developer
  def PRIMARY_KEY = login

  val login = "login".TEXT.NOT_NULL
  def projects = inverseMany(Membership.developer)
}

object Developer extends Developer with Table[String, Developer]

class Project extends Record[String, Project] {
  def relation = Project
  def PRIMARY_KEY = name

  val name = "name".TEXT.NOT_NULL
  def members = inverseMany(Membership.project)
}

object Project extends Project with Table[String, Project]

class Membership extends Record[(String, String), Membership] {

  def this(project: String, developer: String) = {
    this()
    this.name := project
    this.login := developer
  }

  def relation = Membership
  def PRIMARY_KEY = pk

  val project = "project".TEXT.NOT_NULL.REFERENCES(Project).ON_DELETE(CASCADE)
  def name = project.field
  val developer = "developer".TEXT.NOT_NULL.REFERENCES(Developer).ON_DELETE(CASCADE)
  def login = developer.field
  val pk = (project -> developer)
}

object Membership extends Membership with Table[(String, String), Membership]
