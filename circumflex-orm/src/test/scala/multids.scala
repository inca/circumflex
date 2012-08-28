package ru.circumflex
package orm

import org.scalatest._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.MustMatchers

object FooDB extends ORMConfiguration {
  def name = "foo"
  override lazy val url = "jdbc:h2:mem:foodb"
  override lazy val username = "foo"
  override lazy val password = ""
}

object BarDB extends ORMConfiguration {
  def name = "bar"
  override lazy val url = "jdbc:h2:mem:bardb"
  override lazy val username = "bar"
  override lazy val password = ""
}

@RunWith(classOf[JUnitRunner])
class MultiDataSourceSpec
  extends FreeSpec
  with MustMatchers
  with BeforeAndAfterAll
  with BeforeAndAfterEach {

  override def beforeAll() {
    usingAll(FooDB, BarDB) { conf =>
      new DDLUnit(Country).CREATE()
    }
  }

  override def beforeEach() {
    usingAll(FooDB, BarDB) { conf =>
      DELETE(Country).execute()
    }
  }

  "Multiple data sources" - {
    "handle different queries" in {
      // Create two different records in two different databases (same table names for simplicity)
      using(FooDB) {
        val co = new Country
        co.code := "ch"
        co.name := "Switzerland"
        co.save()
      }
      using(BarDB) {
        val co = new Country
        co.code := "fr"
        co.name := "France"
        co.save()
      }
      // Query each database to ensure that they contain only one of them
      using(FooDB) {
        Country.get("ch").isEmpty must equal (false)
        Country.get("fr").isEmpty must equal (true)
      }
      using(BarDB) {
        Country.get("ch").isEmpty must equal (true)
        Country.get("fr").isEmpty must equal (false)
      }
    }
    "allow transfering records between data sources" in {
      // Create record in FOO
      using(FooDB) {
        val co = new Country
        co.code := "ch"
        co.name := "Switzerland"
        co.save()
      }
      // Now query this record from FOO and store it in BAR
      using(FooDB) {
        val ch = Country.get("ch").get
        using(BarDB) {
          ch.save()
        }
      }
      // Make sure that the record exists in BAR
      using(BarDB) {
        Country.get("ch").isEmpty must equal (false)
      }
    }
  }

}