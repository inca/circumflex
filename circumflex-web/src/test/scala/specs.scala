package ru.circumflex.core.test

import ru.circumflex.core._
import org.specs.runner.JUnit4
import org.specs.Specification

class SpecsTest extends JUnit4(CircumflexCoreSpec)

object 
CircumflexCoreSpec extends Specification {

  class MainRouter extends RequestRouter {
    // Common stuff
    get("/") = "preved"
    get("/ctx") = if (!CircumflexContext.live_?) "null" else ctx.toString
    get("/capture/(.*)"r) = "uri$1 is " + uri(1)
    get("/decode me") = "preved"
    post("/post") = "preved"
    put("/put") = "preved"
    delete("/delete") = "preved"
    options("/options") = "preved"
    get("/redirect") = redirect("/")
    get("/rewrite") = rewrite("/")
    get("/error") = error(503, "preved")
    get("/contentType\\.(.+)"r) = {
      ctx.contentType = uri.get(1) match {
        case Some("html") => "text/html"
        case Some("css") => "text/css"
        case _ => "application/octet-stream"
      }
      done()
    }
    // Composite matchers
    get("/composite" & Accept("text/:format") & Referer("localhost")) =
        "3 conditions met (" + param("format") + ")"
    get("/composite" & Accept("text/:format")) =
        "2 conditions met (" + param("format") + ")"
    get("/composite") = "1 condition met"
    // Flashes
    get("/flash-set") = {
      flash('notice) = "preved"
      done()
    }
    get("/flash-get") = flash.get('notice) match {
      case Some(s: String) => s
      case None => ""
    }
    // Simple urls
    get("/filename/:name.:ext") = uri('name) + uri('ext)
    get("*/one/:two/+.:three") =
      uri(1) + uri('two) + uri(3) + uri('three)
    // Messages
    get("/msg") = msg("hello")
    get("/msg/:name") = msg("parameterizedHello", "name" -> uri('name))
  }

  doBeforeSpec{
    Circumflex("cx.router") = classOf[MainRouter]
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
    "match composite routes" in {
      MockApp.get("/composite")
          .setHeader("Accept","text/html")
          .setHeader("Referer","localhost")
          .execute().getContent must_== "3 conditions met (html)"
      MockApp.get("/composite")
          .setHeader("Accept","text/plain")
          .execute().getContent must_== "2 conditions met (plain)"
      MockApp.get("/composite")
          .setHeader("Accept","application/xml")
          .setHeader("Referer","localhost")
          .execute().getContent must_== "1 condition met"
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
    "process messages" in {
      MockApp.get("/msg").execute().getContent must_== "Hello!"
      MockApp.get("/msg")
          .setHeader("Accept-Language", "ru,en;q=0.8,en-us;q=0.2")
          .execute().getContent must_== "Hello!"
      MockApp.get("/msg")
          .setHeader("Accept-Language", "pt,br;q=0.8,en-us;q=0.2")
          .execute().getContent must_== "Hola!"
      MockApp.get("/msg/world")
          .setHeader("Accept-Language", "pt,br;q=0.8,en-us;q=0.2")
          .execute().getContent must_== "Hello, world!"
    }
  }

  "UriMatcher" should {
    "match simplified request 1" in {
      MockApp.get("/filename/file.txt").execute.getContent must_== "filetxt"
    }
    "match simplified request 2" in {
      MockApp.get("/aaa/one/bbb00/cc.ddd.e").execute.getContent must_== "/aaabbb00cc.ddde"
      MockApp.get("/one/bbb00/cc.ddd.e").execute.getContent must_== "bbb00cc.ddde"
      MockApp.get("/one/bbb00/.ddde").execute.getStatus must_== 404
    }
  }

  "CircumflexContext" should {
    "be available thread-locally in Circumflex application scope" in {
      MockApp.get("/ctx").execute().getContent mustNotBe "null"
    }
    "be destroyed after the request processing has finished" in {
      MockApp.get("/").execute
      CircumflexContext.live_? mustBe false
    }
    "contain captured groups from URI" in {
      MockApp.get("/capture/preved").execute().getContent must_== "uri$1 is preved"
    }
    "set response content type" in {
      MockApp.get("/contentType.html").execute()
          .getHeader("Content-Type") must beMatching("text/html(\\s*;\\s*charset=.*)?")
      MockApp.get("/contentType.css").execute()
          .getHeader("Content-Type") must beMatching("text/css(\\s*;\\s*charset=.*)?")
    }
  }

}
