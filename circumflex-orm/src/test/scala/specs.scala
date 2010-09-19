package ru.circumflex.orm

import org.specs.runner.JUnit4
import org.specs.Specification

class SpecsTest extends JUnit4(GeneralSpec)

object GeneralSpec extends Specification {

  "Relations" should {
    "handle equality and canEqual" in {
      Country must_== new Country().relation
      City must_!= new Country().relation
      City.canEqual(new City().relation) must beTrue
      Country.canEqual(new City().relation) must beFalse
    }
    "disallow field assignment" in {
      (Country.name := "preved") must throwA[ORMException]
    }
    "initialize metadata" in {
      Country.fields.size must_== 2
      Country.associations.size must_== 0
      Country.constraints.size must_== 1
      Country.indexes.size must_== 1
      City.fields.size must_== 3
      City.associations.size must_== 1
      City.constraints.size must_== 2
      City.indexes.size must_== 0
    }
  }

  "Records" should {
    "handle equality" in {
      val c1 = new City
      val c2 = new City
      c1 must_!= c2
      c1.id := 1l
      c1 must_!= c2
      c2.id := 1l
      c1 must_== c2
      c2.id := 2l
      c1 must_!= c2
      c1.canEqual(c2) must beTrue
      c1.canEqual(new Country) must beFalse
    }
    "handle different identifier generation strategies" in {
      List(IdGen1, IdGen2, IdGen3, IdGen4).foreach { r =>
        new DDLUnit(r).CREATE
        val record = r.recordClass.newInstance
        record.INSERT_!()
        record.transient_? must beFalse
        new DDLUnit(r).DROP
      }
    }
    "handle validation" in {
      new DDLUnit(Country).CREATE
      val c = new Country
      c.code := ""
      c.validate.get.size must_== 2
      c.code := "1"
      c.validate.get.size must_== 1
      c.validate_! must throwA[ValidationException]
      
      new DDLUnit(Country).DROP
    }
  }

  "Fields" should {
    "handle equality" in {
      new City().name must_== new City().name
      new Country().name must_!= new City().name
      City.name must_== City.name
      City.name must_!= Country.name
    }
    "process setters" in {
      val c = new Country
      c.code := " CH  "
      c.code() must_== "ch"
    }
  }

  "Relation nodes" should {
    "handle equality" in {
      val co = Country AS "co"
      val ci = City AS "ci"
      co must_== (Country AS "co")
      ci must_!= (City AS "c")
      (co JOIN ci) must_== ((co JOIN ci) JOIN (ci JOIN co))
      (co JOIN ci) must_!= (ci JOIN co)
    }
    "join with left-associativity" in {
      val co = Country AS "co"
      val ci = City AS "ci"
      val ci1 = City AS "ci1"
      (co JOIN ci JOIN ci1).toString must_== ((co JOIN ci) JOIN ci1).toString
    }
  }

  "DDL unit" should {
    "be able to create and drop schema" in {
      new DDLUnit(Country).CREATE()
      val ch = new Country("ch", "switzerland")
      ch.INSERT_!() must_== 1
      new DDLUnit(Country).DROP()
      ch.INSERT_!() must throwA[Throwable]
    }
  }

}


// Service classes

class IdGen1 extends Record[Long, IdGen1] with IdentityGenerator[Long, IdGen1] {
  val id = "id".BIGINT.AUTO_INCREMENT
  def relation = IdGen1
  def PRIMARY_KEY = id
}

object IdGen1 extends IdGen1 with Table[Long, IdGen1]

class IdGen2 extends Record[Long, IdGen2] with IdentityGenerator[Long, IdGen2] {
  val id = "id".BIGINT.AUTO_INCREMENT
  def relation = IdGen2
  def PRIMARY_KEY = id
}

object IdGen2 extends IdGen2 with Table[Long, IdGen2] {
  override def autorefresh_?(): Boolean = true
}

class IdGen3 extends Record[Long, IdGen3] with SequenceGenerator[Long, IdGen3] {
  val id = "id".BIGINT.AUTO_INCREMENT
  def relation = IdGen3
  def PRIMARY_KEY = id
}

object IdGen3 extends IdGen3 with Table[Long, IdGen3]

class IdGen4 extends Record[Long, IdGen4] with SequenceGenerator[Long, IdGen4] {
  val id = "id".BIGINT.AUTO_INCREMENT
  def relation = IdGen4
  def PRIMARY_KEY = id
}

object IdGen4 extends IdGen4 with Table[Long, IdGen4] {
  override def autorefresh_?(): Boolean = true
}
