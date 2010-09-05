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

  def selects = {
    val ci = City as "ci"
    val co = Country as "co"
    // Select countries with corresponding cities:
    val s1 = SELECT (co.*, ci.*) FROM (co JOIN ci) list // Seq[(Country, City)]
    // Select countries and count their cities:
    val s2 = SELECT (co.*, COUNT(ci.id)) FROM (co JOIN ci) GROUP_BY (co.*) list // Seq[(Country, Int)]
    // Select all russian cities:
    val s3 = SELECT (ci.*) FROM (ci JOIN co) WHERE (co.code LIKE "ru") ORDER_BY (ci.name ASC) list  // Seq[City]
  }
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
  }

}