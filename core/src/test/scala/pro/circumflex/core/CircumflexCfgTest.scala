package pro.circumflex
package core

import org.specs2.mutable.SpecificationWithJUnit

class CircumflexCfgTest extends SpecificationWithJUnit {

  "Circumflex configuration" should {
    "take parameters from cx.properties from classpath" in {
      cx.getString("test") must_== Some("Hello world!")
    }
    "instantiate an object" in {
      // Instantiation
      cx.instantiate[Dummy]("dummy", Dummy).toString must_==
          "Simple dummy instance"
      cx.instantiate[Dummy]("nop", Dummy).toString must_==
          "Dummy singleton"
    }
  }

}
