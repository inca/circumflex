package ru.circumflex.core

import org.specs.runner.JUnit4
import org.specs.Specification
import java.util.UUID

class Dummy {
  val uuid = UUID.randomUUID.toString
  override def equals(obj: Any): Boolean = obj match {
    case d: Dummy => this.uuid.equals(d.uuid)
    case _ => false
  }
  override def hashCode: Int = uuid.hashCode
}

object DefaultDummy extends Dummy

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

  // Circumflex Messages API
  "Circumflex Messages" should {
    "resolve messages in different locales" in {
      ctx("cx.locale") = "en_US"
      msg("hello") must_== "Hello!"
      ctx("cx.locale") = "pt"
      msg("hello") must_== "Hola!"
    }
    "format messages using `MessageFormat`" in {
      msg.format("formatHello", "dude") must_== "Hello, dude!"
    }
    "format messages using simple interpolations" in {
      msg.fmt("fmtHello", "name" -> "dude") must_== "Hello, dude!"
    }
  }

}
