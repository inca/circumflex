package ru.circumflex
package orm

import org.scalatest._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.MustMatchers
import scala.xml._
import core._

object Sample {
  def createSchema() = new DDLUnit(Country, City, Capital, Developer, Project, Membership).CREATE()
  def loadData() {
    Deployment.readAll(XML.load(getClass.getResourceAsStream("/test.cxd.xml")))
        .foreach(_.process())
  }
  def dropSchema = new DDLUnit(Country, City, Capital, Developer, Project, Membership).DROP()
}

@RunWith(classOf[JUnitRunner])
class BasicSpec
  extends FreeSpec
  with MustMatchers
  with BeforeAndAfter {

  val ci = City AS "ci"
  val co = Country AS "co"

  before {
    Sample.createSchema()
    Sample.loadData()
  }

  after {
    Sample.dropSchema
  }

  "Relations" - {
    "handle equality" in {
      Country must equal (new Country().relation)
      City must not equal (new Country().relation)
      City.canEqual(new City().relation) must equal (true)
      Country.canEqual(new City().relation) must equal (false)
    }
    "initialize metadata" in {
      Country.fields.size must equal (2)
      Country.associations.size must equal (0)
      Country.constraints.size must equal (0)
      Country.indexes.size must equal (1)
      City.fields.size must equal (3)
      City.associations.size must equal (1)
      City.constraints.size must equal (2)
      City.indexes.size must equal (0)
    }
  }

  "Records" - {
    "handle equality" in {
      val c1 = new Country
      val c2 = new Country
      c1 must not equal (c2)
      c1.code := "ch"
      c1 must not equal (c2)
      c2.code := "ch"
      c1 must equal (c2)
      c2.code := "fr"
      c1 must not equal (c2)
      c1.canEqual(c2) must equal (true)
      c1.canEqual(new City) must equal (false)
    }
    "handle different identifier generation strategies" in {
      List(IdentNoAuto, IdentAuto, SeqNoAuto, SeqAuto).foreach { r =>
        try {
          new DDLUnit(r).CREATE()
          val record = r.recordClass.newInstance
          record.INSERT_!()
          record.isTransient must equal (false)
          new DDLUnit(r).DROP()
        } catch {
          case e: UnsupportedOperationException =>
            ORM_LOG.warn("Database does not support one of identity generation strategies.")
        }
      }
    }
    "handle validation" in {
      val c = new Country
      c.code := ""
      c.validate().get.size must equal (2)
      c.code := "1"
      c.validate().get.size must equal (1)
      evaluating { c.validate_! } must produce [ValidationException]
    }
    "handle decimal types" in {
      new DDLUnit(DecimalRecord).CREATE()
      val r = new DecimalRecord
      r.value := BigDecimal("13.653214")
      r.INSERT()
      r.value() must equal (BigDecimal("13.6532"))
      new DDLUnit(DecimalRecord).DROP()
    }
  }

  "Fields" - {
    "handle equality" in {
      new City().name must equal (new City().name)
      new Country().name must not equal (new City().name)
      City.name must equal (City.name)
      City.name must not equal (Country.name)
    }
    "process setters" in {
      val c = new Country
      c.code := " CH  "
      c.code() must equal ("ch")
    }
  }

  "Relation nodes" - {
    "handle equality" in {
      val co = Country AS "co"
      val ci = City AS "ci"
      co must equal (Country AS "co")
      ci must not equal (City AS "c")
      (co JOIN ci) must equal ((co JOIN ci) JOIN (ci JOIN co))
      (co JOIN ci) must not equal (ci JOIN co)
    }
    "join with left-associativity" in {
      val co = Country AS "co"
      val ci = City AS "ci"
      val ci1 = City AS "ci1"
      (co JOIN ci JOIN ci1).toString must equal (((co JOIN ci) JOIN ci1).toString)
    }
  }

  "Composite key records" - {
    "handle simple querying and equality" in {
      Membership.get("nuke'em" -> "joe") must equal (Some(new Membership("nuke'em", "joe")))
    }
    "handle associations" in {
      // inverse
      Developer.get("bob").get.projects.get.size must equal (2)
      // straight
      Membership.get("heal'em" -> "greg").get.developer().login() must equal ("greg")
    }
  }

  "Querying API" - {
    "handle simple selects" in {
      SELECT(co.*).FROM(co).list().size must equal (3)
    }
    "handle distincts, joins and predicates" in {
      SELECT(co.*)
          .DISTINCT
          .FROM(co JOIN ci)
          .WHERE(ci.name LIKE "Lausanne")
          .unique()
          .get
          .code() must equal ("ch")
    }
    "handle projections" in {
      SELECT(COUNT(ci.id)).FROM(ci JOIN co).WHERE(co.code LIKE "ch").unique().get must equal (3l)
    }
    "handle unions" in {
      SELECT(ci.name).FROM(ci).UNION(SELECT(co.name).FROM(co)).list().size must equal (10)
    }
    "handle subqueries" in {
      val q = SELECT(ci.country.field).FROM(ci).WHERE(ci.name LIKE "Lausanne")
      SELECT(co.*).FROM(co).WHERE(co.code IN q).unique().get.code() must equal ("ch")
    }
    "handle limits, offsets and order-by's" in {
      SELECT(co.*).FROM(co).LIMIT(1).OFFSET(1).ORDER_BY(co.name ASC).unique().get.code() must equal ("ch")
    }
    "handle criteria and inverse association merging" in {
      val ch = Country.get("ch").get
      val l = (City.byName("%u%") AND ch.cities).list()
      l.size must equal (2)
      l(0).name() must equal ("Lausanne")
      l(1).name() must equal ("Zurich")
    }
    "handle native SQL" in {
      val p = expr[String]("c.name")
      val q = "SELECT {*} FROM orm.country c where c.code LIKE :code".toSql(p)
      q.set("code", "ch").unique.get must equal ("Switzerland")
      q.set("code", "ru").unique.get must equal ("Russia")
    }
    "handle native DML" in {
      "UPDATE orm.country c SET c.code = c.code".toDml.execute() must equal (3)
    }
  }

  "Transaction API" - {
    "handle rollbacks" in {
      val pt = new Country("pt", "Portugal")
      pt.INSERT()
      SELECT(co.*).FROM(co).WHERE(co.code LIKE "pt").unique must equal (Some(pt))
      ROLLBACK()
      SELECT(co.*).FROM(co).WHERE(co.code LIKE "pt").unique must equal (None)
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
      } catch { case e: Exception => }
      SELECT(co.*).FROM(co).WHERE(co.name LIKE "Test").list().size must equal (3)
      ROLLBACK()
      SELECT(co.*).FROM(co).WHERE(co.name LIKE "Test").list().size must equal (0)
    }
  }

}
