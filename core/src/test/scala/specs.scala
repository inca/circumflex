package circumflex
package core

import java.util.UUID
import org.scalatest._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.MustMatchers

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

@RunWith(classOf[JUnitRunner])
class CircumflexCoreSpecs
  extends FreeSpec
  with MustMatchers {

  // Circumflex Configuration API

  "Circumflex Configuration" - {
    "take params from `cx.properties`" in {
      cx("test") must equal ("preved")
    }
    "provide object insantiation facility" in {
      val dummy = cx.instantiate[Dummy]("dummy", DefaultDummy)
      dummy must not be (null)
      dummy must not be equal (cx.instantiate[Dummy]("dummy", DefaultDummy))
    }
    "instantiate singletons" in {
      cx.instantiate[Dummy]("defaultDummy") must be theSameInstanceAs (cx.instantiate[Dummy]("defaultDummy"))
    }
  }

  // Circumflex Context API

  "Circumflex Context" - {
    "be initialized and destroyed properly" in {
      Context.init()
      Context.isLive must equal (true)
      Context.destroy()
      Context.isLive must equal (false)
    }
    "initialize on demand" in {
      Context.get must not be (null)
      Context.isLive must equal (true)
    }
    "provide DSL for setting and getting context variables" in {
      'test := "preved"
      ctx.get("test") must equal (Some("preved"))
    }
    "process finalizers" in {
      var i = 1
      Context.executeInNew { ctx =>
        i = 4
        ctx.enqueueFinalizer("key1", () => i += 1)
        ctx.enqueueFinalizer("key1", () => i += 1)
        ctx.enqueueFinalizer("key2", () => i += 1)
        ctx.pushFinalizer("first", () => i *= 2)
        ctx.pushFinalizer("first", () => i *= 2)
      }
      i must equal(10)
    }
  }

  // Circumflex Messages API

  "Circumflex Messages" - {
    "resolve messages in different locales" in {
      ctx("cx.locale") = "en_US"
      msg("hello") must equal ("Hello!")
      msg("test") must equal("Preved!")
      ctx("cx.locale") = "pt"
      msg("hello") must equal ("Hola!")
      msg("test") must equal ("Preved!")
    }
    "format messages using `MessageFormat`" in {
      msg.format("formatHello", "dude") must equal ("Hello, dude!")
    }
    "format messages using simple interpolations" in {
      msg.fmt("fmtHello", "name" -> "dude") must equal ("Hello, dude!")
    }
  }

}