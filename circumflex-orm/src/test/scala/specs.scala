package ru.circumflex.orm

import org.specs.runner.JUnit4
import org.specs.Specification

class SpecsTest extends JUnit4(GeneralSpec)

object GeneralSpec extends Specification {

  "Relations" should {
    "handle equality" in {
      Country must_== new Country().relation
      City must_!= new Country().relation
    }
    "disallow field assignment" in {
      (Country.name := "preved") must throwA[ORMException]
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
    }
    "process field setters" in {
      val c = new Country
      c.code := " CH  "
      c.code() must_== "ch"
    }
  }

  

}