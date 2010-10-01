package ru.circumflex.orm

import org.specs.runner.JUnit4
import org.specs.Specification
import xml._

object Sample {
  def createSchema = new DDLUnit(Country, City, Capital).CREATE
  def loadData = Deployment.readAll(XML.load(getClass.getResourceAsStream("/test.cxd.xml")))
        .foreach(_.process)
  def dropSchema = new DDLUnit(Country, City, Capital).DROP
}

class SpecsTest extends JUnit4(CircumflexORMSpec)

object CircumflexORMSpec extends Specification {

  val ci = City AS "ci"
  val co = Country AS "co"

  doBeforeSpec {
    Sample.createSchema
    Sample.loadData
  }

  doAfterSpec {
    Sample.dropSchema
  }

  "Relations" should {
    "handle equality" in {
      Country must_== new Country().relation
      City must_!= new Country().relation
      City.canEqual(new City().relation) must beTrue
      Country.canEqual(new City().relation) must beFalse
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
      List(IdentNoAuto, IdentAuto, SeqNoAuto, SeqAuto).foreach { r =>
        new DDLUnit(r).CREATE
        val record = r.recordClass.newInstance
        record.INSERT_!()
        record.transient_? must beFalse
        new DDLUnit(r).DROP
      }
    }
    "handle validation" in {
      val c = new Country
      c.code := ""
      c.validate.get.size must_== 2
      c.code := "1"
      c.validate.get.size must_== 1
      c.validate_! must throwA[ValidationException]
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

  "Querying API" should {
    "handle simple selects" in {
      SELECT(co.*).FROM(co).list.size must_== 3
    }
    "handle distincts, joins and predicates" in {
      SELECT(co.*)
          .DISTINCT
          .FROM(co JOIN ci)
          .WHERE(ci.name LIKE "Lausanne")
          .unique.get.code() must_== "ch"
    }
    "handle projections" in {
      SELECT(COUNT(ci.id)).FROM(ci JOIN co).WHERE(co.code LIKE "ch").unique.get must_== 3l
    }
    "handle unions" in {
      SELECT(ci.name).FROM(ci).UNION(SELECT(co.name).FROM(co)).list.size must_== 10
    }
    "handle subqueries" in {
      val q = SELECT(ci.country.field).FROM(ci).WHERE(ci.name LIKE "Lausanne")
      SELECT(co.*).FROM(co).WHERE(co.code IN q).unique.get.code() must_== "ch"
    }
    "handle limits, offsets and order-by's" in {
      SELECT(co.*).FROM(co).LIMIT(1).OFFSET(1).ORDER_BY(co.name ASC).unique.get.code() must_== "ch"
    }
    "handle criteria and inverse association merging" in {
      val ch = Country.get("ch").get
      val l = (City.byName("%u%") AND ch.cities).list
      l.size must_== 2
      l(0).name() must_== "Lausanne"
      l(1).name() must_== "Zurich"
    }
  }

  "Transaction API" should {
    "handle rollbacks" in {
      val pt = new Country("pt", "Portugal")
      pt.INSERT()
      SELECT(co.*).FROM(co).WHERE(co.code LIKE "pt").unique must_== Some(pt)
      ROLLBACK
      SELECT(co.*).FROM(co).WHERE(co.code LIKE "pt").unique must_== None
    }
    "handle nested transactions" in {
      // we emulate the conditions where only 3 of 10 insert operations are successful
      for (i <- 0 until 10) try tx {
        val code = i match {
          case 0 => "xx"
          case 1 => "yy"
          case 6 => "zz"
          case 2 => ""    // won't pass validation => fail
          case _ => "xx"  // already exist => fail
        }
        new Country(code, "Test").INSERT()
      } catch { case _ => }
      SELECT(co.*).FROM(co).WHERE(co.name LIKE "Test").list.size must_== 3
      ROLLBACK
      SELECT(co.*).FROM(co).WHERE(co.name LIKE "Test").list.size must_== 0
    }
  }

}


// Classes for testing Identifier Generation Strategies

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
  override def autorefresh_?(): Boolean = true
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
  override def autorefresh_?(): Boolean = true
}
