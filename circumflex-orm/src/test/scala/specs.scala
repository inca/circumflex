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
  }

}