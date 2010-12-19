package ru.circumflex.scalate

import ru.circumflex._, core._, web._
import org.specs.runner.JUnit4
import org.specs.Specification

class SpecsTest extends JUnit4(CircumflexScalateSpec)

class MockRouter extends RequestRouter {

  get("/hello/:name") = {
    'name := param("name")
    render("/hello.ssp", 200, false)
  }

}

object CircumflexScalateSpec extends Specification {

  doBeforeSpec {
    cx("cx.router") = classOf[MockRouter]
    cx("cx.root") = "src/test/resources"
    MockApp.start
  }

  "Circumflex Scalate Views" should {
    "render simple layouts" in {
      MockApp.get("/hello/Hiram").execute().getContent must_== "<p>Hello, Hiram!</p>"
    }
  }

  doAfterSpec { MockApp.stop }

}
