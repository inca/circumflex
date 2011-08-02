package ru.circumflex
package orm

// Generic test model

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
  val nameIdx = "name_idx".INDEX("name")

  validation.notEmpty(_.code).pattern(_.code, "^[a-z]{2}$", "syntax")
}

class City extends Record[String, City] {
  def this(name: String, country: Country) = {
    this()
    this.name := name
    this.country := country
  }

  val id = "id".TEXT.NOT_NULL(java.util.UUID.randomUUID.toString)
  val name = "name".TEXT.NOT_NULL
  val country = "country_code".VARCHAR(2).NOT_NULL.REFERENCES(Country).ON_DELETE(CASCADE)

  def PRIMARY_KEY = id
  def relation = City
  override def toString = name.getOrElse("<unknown>")
}

object City extends City with Table[String, City] with Cacheable[String, City] {
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
  val city = "city_id".TEXT.NOT_NULL.REFERENCES(City).ON_DELETE(CASCADE)

  def relation = Capital
  def PRIMARY_KEY = country.field
  override def toString = city().name.getOrElse("<unknown>")
}

object Capital extends Capital with Table[String, Capital] with Cacheable[String, Capital] {
  val cityKey = UNIQUE(city)
}

// Field Composition test model

class Developer extends Record[String, Developer] {
  def relation = Developer
  def PRIMARY_KEY = login

  val login = "login".VARCHAR(255).NOT_NULL
  def projects = inverseMany(Membership.developer)
}

object Developer extends Developer with Table[String, Developer]

class Project extends Record[String, Project] {
  def relation = Project
  def PRIMARY_KEY = name

  val name = "name".VARCHAR(255).NOT_NULL
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

  val project = "project".VARCHAR(255).NOT_NULL.REFERENCES(Project).ON_DELETE(CASCADE)
  def name = project.field
  val developer = "developer".VARCHAR(255).NOT_NULL.REFERENCES(Developer).ON_DELETE(CASCADE)
  def login = developer.field
  val pk = composition(name, login)

}

object Membership extends Membership with Table[(String, String), Membership]

// Identifier Generation Strategies test model

object IdGen extends Schema("idgen")

class IdentNoAuto extends Record[Long, IdentNoAuto] with IdentityGenerator[Long, IdentNoAuto] {
  val id = "id".BIGINT.AUTO_INCREMENT
  def relation = IdentNoAuto
  def PRIMARY_KEY = id
}

object IdentNoAuto extends IdentNoAuto with Table[Long, IdentNoAuto] {
  override def schema: Schema = IdGen
}

class IdentAuto extends Record[Long, IdentAuto] with IdentityGenerator[Long, IdentAuto] {
  val id = "id".BIGINT.AUTO_INCREMENT
  def relation = IdentAuto
  def PRIMARY_KEY = id
}

object IdentAuto extends IdentAuto with Table[Long, IdentAuto] {
  override def schema: Schema = IdGen
  override def isAutoRefresh: Boolean = true
}

class SeqNoAuto extends Record[Long, SeqNoAuto] with SequenceGenerator[Long, SeqNoAuto] {
  val id = "id".BIGINT.AUTO_INCREMENT
  def relation = SeqNoAuto
  def PRIMARY_KEY = id
}

object SeqNoAuto extends SeqNoAuto with Table[Long, SeqNoAuto] {
  override def schema: Schema = IdGen
}

class SeqAuto extends Record[Long, SeqAuto] with SequenceGenerator[Long, SeqAuto] {
  val id = "id".BIGINT.AUTO_INCREMENT
  def relation = SeqAuto
  def PRIMARY_KEY = id
}

object SeqAuto extends SeqAuto with Table[Long, SeqAuto] {
  override def schema: Schema = IdGen
  override def isAutoRefresh: Boolean = true
}

// SQL Types test model

object DecimalSchema extends Schema("decimal")

class DecimalRecord extends Record[BigDecimal, DecimalRecord] {
  val value = "value".NUMERIC(12, 4).NOT_NULL
  def PRIMARY_KEY = value
  def relation = DecimalRecord
}

object DecimalRecord extends DecimalRecord with Table[BigDecimal, DecimalRecord] {
  override def schema = DecimalSchema
  override def isAutoRefresh: Boolean = true
}