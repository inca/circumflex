package ru.circumflex
package orm

import org.specs.Specification

object FooDB extends ORMConfiguration {
  def name = "foo"
  override val url = "jdbc:h2:mem:foodb"
  override val username = "foo"
  override val password = ""
}

object BarDB extends ORMConfiguration {
  def name = "bar"
  override val url = "jdbc:h2:mem:bardb"
  override val username = "bar"
  override val password = ""
}

object MultiDataSourceSpec extends Specification {

  doBeforeSpec {
    usingAll(FooDB, BarDB) { conf =>
      new DDLUnit(Country).CREATE()
    }
  }

  new SpecContext {
    beforeExample {
      usingAll(FooDB, BarDB) { conf =>
        DELETE(Country).execute()
      }
    }
  }

  "Multiple data sources" should {
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
        Country.get("ch").isEmpty mustBe false
        Country.get("fr").isEmpty mustBe true
      }
      using(BarDB) {
        Country.get("ch").isEmpty mustBe true
        Country.get("fr").isEmpty mustBe false
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
        Country.get("ch").isEmpty mustBe false
      }
    }
  }

}