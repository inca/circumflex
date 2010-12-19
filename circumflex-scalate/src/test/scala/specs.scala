package ru.circumflex.scalate

import ru.circumflex._, core._, web._
import org.specs.runner.JUnit4
import org.specs.Specification

class SpecsTest extends JUnit4(CircumflexScalateSpec)

class MockRouter extends RequestRouter {

  get("/hello.ssp/:name") = {
    'name := param("name")
    render("/hello.ssp", 200, false)
  }
  
  get("/hello.scaml/:name") = {
    'name := param("name")
    render("/hello.scaml", 200, false)
  }
  
  get("/hello.jade/:name") = {
    'name := param("name")
    render("/hello.jade", 200, false)
  }

}

object CircumflexScalateSpec extends Specification {

  doBeforeSpec {
    cx("cx.router") = classOf[MockRouter]
    cx("cx.root") = "src/test/resources"
    MockApp.start
  }

  "Circumflex Scalate Views" should {
    for(kind <- List("ssp", "scaml", "jade")) {
      "render simple "+kind+" templates" in {
        MockApp.get("/hello."+kind+"/Hiram").execute().getContent.trim must_== "<p>Hello, Hiram!</p>"
      }
    }
  }

  doAfterSpec { MockApp.stop }

}
