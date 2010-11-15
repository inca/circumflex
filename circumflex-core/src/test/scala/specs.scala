package ru.circumflex.core

import org.specs.runner.JUnit4
import org.specs.Specification
import java.util.UUID

// Mocks

class Dummy {
  val uuid = UUID.randomUUID.toString
  override def equals(obj: Any): Boolean = obj match {
    case d: Dummy => this.uuid.equals(d.uuid)
    case _ => false
  }
  override def hashCode: Int = uuid.hashCode
}

object DefaultDummy extends Dummy

class CustomContext extends Context

// Specs

class SpecsTest extends JUnit4(CircumflexCoreSpec)

object CircumflexCoreSpec extends Specification {

  // Circumflex Configuration API

  "Circumflex Configuration" should {
    "take params from `cx.properties`" in {
      cx("test") must_== "preved"
    }
    "provide object insantiation facility" in {
      val dummy = cx.instantiate[Dummy]("dummy", DefaultDummy)
      dummy must notBeNull
      dummy must_!= cx.instantiate[Dummy]("dummy", DefaultDummy)
    }
    "instantiate singletons" in {
      cx.instantiate[Dummy]("defaultDummy") must_== cx.instantiate[Dummy]("defaultDummy")
    }
  }

  // Circumflex Context API

  "Circumflex Context" should {
    "be initialized and destroyed properly" in {
      Context.live_? must beFalse
      Context.init()
      Context.live_? must beTrue
      Context.destroy()
      Context.live_? must beFalse
    }
    "initialize on demand" in {
      Context.live_? must beFalse
      Context.get must notBeNull
      Context.live_? must beTrue
    }
    "process events" in {
      var inits = 0;
      var destroys = 0;
      Context.addInitListener(c => inits += 1)
      Context.addDestroyListener(c => destroys += 1)
      Context.init()
      Context.destroy()
      Context.init()
      Context.destroy()
      Context.init()
      Context.destroy()
      inits must_== 3
      destroys must_== 3
    }
    "be configurable" in {
      cx("cx.context") = classOf[CustomContext]
      ctx.isInstanceOf[CustomContext] must beTrue
    }
    "provide DSL for setting and getting context variables" in {
      'test := "preved"
      'test.get must_== Some("preved")
    }
    "provide untyped container functionality" in {
      'dummy := DefaultDummy
      'dummy.apply[Dummy].uuid must_== DefaultDummy.uuid
      'testInt := "0"
      'testInt.getInt must_== 0
      'testBoolean := "false"
      'testBoolean.getBoolean must_== false
      'testDate := "29.01.1988 +0300"
      'testDate.getDate("dd.MM.yyyy ZZ").getTime must_== 570402000000l
    }
  }

  // Circumflex Messages API

  "Circumflex Messages" should {
    "resolve messages in different locales" in {
      ctx("cx.locale") = "en_US"
      msg("hello") must_== "Hello!"
      msg("test") must_== "Preved!"
      ctx("cx.locale") = "pt"
      msg("hello") must_== "Hola!"
      msg("test") must_== "Preved!"
    }
    "format messages using `MessageFormat`" in {
      msg.format("formatHello", "dude") must_== "Hello, dude!"
    }
    "format messages using simple interpolations" in {
      msg.fmt("fmtHello", "name" -> "dude") must_== "Hello, dude!"
    }
  }

}
