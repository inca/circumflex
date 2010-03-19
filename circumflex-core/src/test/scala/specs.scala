package ru.circumflex.core.test

import ru.circumflex.core._
import org.specs.runner.JUnit4
import org.specs.Specification

class SpecsTest extends JUnit4(CircumflexCoreSpec)

object CircumflexCoreSpec extends Specification {

  class MainRouter extends RequestRouter {
    get("/") = "preved"
    get("/ctx") = if (ctx == null) "null" else ctx.toString
    get("/capture/?", headers("Accept" -> "([^/]+)/([^/]+)")) =
        "Accept$1 is " + param("Accept$1").getOrElse("") + "; " +
            "Accept$2 is " + param("Accept$2").getOrElse("")
    get("/capture/(.*)") = "uri$1 is " + param("uri$1").getOrElse("")
    get("/decode me") = "preved"
    post("/post") = "preved"
    put("/put") = "preved"
    delete("/delete") = "preved"
    options("/options") = "preved"
    get("/redirect") = redirect("/")
    get("/rewrite") = rewrite("/")
    get("/error") = error(503, "preved")
    get("/contentType\\.(.+)") = {
      ctx.contentType = param("uri$1") match {
        case Some("html") => "text/html"
        case Some("css") => "text/css"
        case _ => "application/octet-stream"
      }
      done(200)
    }
    get("/flash-set") = {
      flash("notice") = "preved"
      done(200)
    }
    get("/flash-get") = flash("notice") match {
      case Some(s: String) => s
      case None => ""
    }
  }

  doBeforeSpec{
    Circumflex.cfg("cx.router") = classOf[MainRouter]
    MockApp.start
  }

  doAfterSpec { MockApp.stop }

  "RequestRouter" should {
    "match the request against it's routes until first match" in {
      MockApp.get("/").execute().getContent must_== "preved"
    }
    "return to the filter if no routes match (default filter's behavior is 404)" in {
      MockApp.get("/this/does/not/match/any/routes").execute().getStatus must_== 404
    }
    "decode URIs before matching" in {
      MockApp.get("/decode%20me").execute().getContent must_== "preved"
    }
    "match POST requests" in {
      MockApp.post("/post").execute().getContent must_== "preved"
    }
    "match PUT requests" in {
      MockApp.put("/put").execute().getContent must_== "preved"
    }
    "match DELETE requests" in {
      MockApp.delete("/delete").execute().getContent must_== "preved"
    }
    "match OPTIONS requests" in {
      MockApp.options("/options").execute().getContent must_== "preved"
    }
    "interpret '_method' parameter as HTTP method" in {
      MockApp.get("/put?_method=PUT").execute().getContent must_== "preved"
      MockApp.post("/put")
          .setContent("_method=PUT")
          .execute()
          .getContent must_== "preved"
    }
    "send redirects" in {
      val r = MockApp.get("/redirect").execute()
      r.getStatus must_== 302
      r.getHeader("Location") must_== "http://localhost/"
    }
    "process rewrites" in {
      MockApp.get("/rewrite").execute().getContent must_== "preved"
    }
    "process errors" in {
      MockApp.get("/error").execute().getStatus must_== 503
    }
  }

  "CircumflexContext" should {
    "be available thread-locally in Circumflex application scope" in {
      MockApp.get("/ctx").execute().getContent mustNotBe "null"
    }
    "be destroyed after the request processing has finished" in {
      MockApp.get("/").execute
      Circumflex.ctx mustBe null
    }
    "contain captured groups from URI" in {
      MockApp.get("/capture/preved").execute().getContent must_== "uri$1 is preved"
    }
    "contain captured groups from headers" in {
      MockApp.get("/capture")
          .setHeader("Accept", "text/plain")
          .execute()
          .getContent must_== "Accept$1 is text; Accept$2 is plain"
    }
    "set response content type" in {
      MockApp.get("/contentType.html").execute()
          .getHeader("Content-Type") must beMatching("text/html(\\s*;\\s*charset=.*)?")
      MockApp.get("/contentType.css").execute()
          .getHeader("Content-Type") must beMatching("text/css(\\s*;\\s*charset=.*)?")
    }


  }

}
