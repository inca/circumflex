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

  

}