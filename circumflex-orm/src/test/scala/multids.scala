package ru.circumflex
package orm

import org.specs.Specification
import org.specs.runner.JUnit4

object FooDB extends SimpleORMConfiguration {
  def url = "jdbc:h2:mem:foodb"
  def username = "foo"
  def password = ""
}

object BarDB extends SimpleORMConfiguration {
  def url = "jdbc:h2:mem:bardb"
  def username = "bar"
  def password = ""
}

class MultiDataSourceSpec extends JUnit4(MultiDataSourceSpec)

object MultiDataSourceSpec extends Specification {
  doBeforeSpec {
    usingAll(FooDB, BarDB) { conf =>
      DDLUnit.fromClasspath().CREATE()
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
        COMMIT()
      }
      using(BarDB) {
        val co = new Country
        co.code := "fr"
        co.name := "France"
        co.save()
        COMMIT()
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
      // Cleanup all databases
      usingAll(FooDB, BarDB) { conf =>
        DELETE(Country).execute()
      } must_== Seq(1, 1)
    }
    "allow transfering records between data sources" in {
      // Create record in FOO
      using(FooDB) {
        val co = new Country
        co.code := "ch"
        co.name := "Switzerland"
        co.save()
        COMMIT()
      }
      // Now query this record from FOO and store it in BAR
      using(FooDB) {
        val ch = Country.get("ch").get
        using(BarDB) {
          ch.save()
          COMMIT()
        }
      }
      // Make sure that the record exists in BAR
      val ch = using(BarDB) { Country.all(0) }
      ch.code() must_== "ch"
      // Cleanup all databases
      usingAll(FooDB, BarDB) { conf =>
        DELETE(Country).execute()
      } must_== Seq(1, 1)
    }
  }

}