package ru.circumflex.orm

import scala.xml._
import org.specs.Specification
import org.specs.runner.JUnit4

class SpecsTest extends JUnit4(CircumflexORMSpec)

object Sample {
  def createSchema = new DDLUnit(City, Capital, Country)
      .create
      .messages
      .foreach(msg => println(msg.body))
  def dropSchema = new DDLUnit(City, Capital, Country)
      .drop
      .messages
      .foreach(msg => println(msg.body))
  def importData = Deployment
      .readAll(XML.load(getClass.getResourceAsStream("/test.cxd.xml")))
      .foreach(_.process)
}

object CircumflexORMSpec extends Specification {

  val ci = City as "ci"
  val co = Country as "co"

  doBeforeSpec {
    Sample.createSchema
    Sample.importData
  }

  doAfterSpec {
    Sample.dropSchema
  }

  "Circumflex ORM" should {
    "do simple selects" >> {
      SELECT(co.*).FROM(co).list.size must_==3
    }
    "process distinct, joins and predicates" >> {
      SELECT(co.*)
          .DISTINCT
          .FROM(co JOIN ci)
          .WHERE(ci.name LIKE "Lausanne")
          .unique.get.code() must_== "ch"
    }
    "process projections" >> {
      SELECT(COUNT(ci.id)).FROM(ci JOIN co).WHERE(co.code LIKE "ru").unique.get must_== 3l
    }
    "process transactions" >> {
      val c = new Country("pt", "Portugal")
      c.save
      Country.byCode("pt") must_== Some(c)
      ROLLBACK
      Country.byCode("pt") must_== None
    }
    "process nested transactions" >> {
      // Let's try to create 10 countries, but emulate
      // the conditions where only 3 operations are successful
      for (i <- 0 until 10) try tx {
        val code = i match {
          case 0 => "xx"
          case 1 => "yy"
          case 2 => "zz"
          case 3 => ""    // won't pass validation => fails
          case _ => "xx"  // already exist => fails
        }
        new Country(code, "Test").save
      } catch { case _ => }
      Country.byName("Test").size must_== 3
      ROLLBACK
      Country.byName("Test").size must_== 0
    }
  }

}