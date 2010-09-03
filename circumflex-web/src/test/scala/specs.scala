package ru.circumflex.web

import ru.circumflex.core._
import matchers._
import org.specs.runner.JUnit4
import org.specs.Specification

class SpecsTest extends JUnit4(CircumflexWebSpec)

class MockRouter extends RequestRouter {
  get("/") = "preved"
  post("/") = "preved from POST"
  get("/decode me") = "preved"
  get("/regex/(.*)".r) = "matched " + uri(1)
  put("/") = "this is a put route"

  get("/error") = error(503)
  get("/redirect") = redirect("/")

  any("/sub/*") = new SubMockRouter

  new MatchingMockRouter
}

class SubMockRouter extends RequestRouter("/sub") {
  get("/") = "preved"
}

class MatchingMockRouter extends RequestRouter("/matching") {
  get("/uri/:name") = "preved, " + uri('name)
  get("/uri/:name.:ext") = uri('name) + "->" + uri('ext)
  get("/uri/*/one/:two/+.:three") = uri(1) + uri('two) + uri(3) + uri('three)

  get("/param" & HOST(":host")) = "host is " + param("host")

  get("/composite" & ACCEPT("text/:format") & REFERER("localhost")) =
          "3 conditions met (" + param("format") + ")"
  get("/composite" & ACCEPT("text/:format")) =
          "2 conditions met (" + param("format") + ")"
  get("/composite") = "1 condition met"
}

object CircumflexWebSpec extends Specification {

  doBeforeSpec{
    cx("cx.router") = classOf[MockRouter]
    MockApp.start
  }

  doAfterSpec { MockApp.stop }

  "RequestRouter" should {
    "return 404 by default on non-matched requests" in {
      MockApp.get("/this/does/not/match/any/routes").execute().getStatus must_== 404
    }
    "decode URIs before matching" in {
      MockApp.get("/decode%20me").execute().getContent must_== "preved"
    }
    "match requests by simple URI" in {
      MockApp.get("/").execute().getContent must_== "preved"
    }
    "match requests by method" in {
      MockApp.post("/").execute().getContent must_== "preved from POST"
    }
    "match requests by regex" in {
      MockApp.get("/regex/piu").execute().getContent must_== "matched piu"
    }
    "interpret `_method` parameter as HTTP method" in {
      MockApp.get("/?_method=PUT").execute().getContent must_== "this is a put route"
      MockApp.post("/")
              .setContent("_method=PUT")
              .execute()
              .getContent must_== "this is a put route"
    }
    "process subrouters" in {
      MockApp.get("/sub/").execute().getContent must_== "preved"
    }
    "send errors" in {
      MockApp.get("/error").execute().getStatus must_== 503
    }
    "send redirects" in {
      MockApp.get("/redirect").execute().getStatus must_== 302
    }
  }

  "Matching mechanism" should {
    "process named parameters from URI" in {
      MockApp.get("/matching/uri/Jack").execute().getContent must_== "preved, Jack"
      MockApp.get("/matching/uri/file.txt").execute().getContent must_== "file->txt"
      MockApp.get("/matching/uri/I/one/Love/Circum.flex").execute().getContent must_== "ILoveCircumflex"
    }
    "process named parameters from current match results, delegating to request parameters on fail" in {
      MockApp.get("/matching/param")
              .setHeader("Host", "preved")
              .execute()
              .getContent must_== "host is preved"
    }
    "match composite routes" in {
      MockApp.get("/matching/composite")
              .setHeader("Accept","text/html")
              .setHeader("Referer","localhost")
              .execute().getContent must_== "3 conditions met (html)"
      MockApp.get("/matching/composite")
              .setHeader("Accept","text/plain")
              .execute().getContent must_== "2 conditions met (plain)"
      MockApp.get("/matching/composite")
              .setHeader("Accept","application/xml")
              .setHeader("Referer","localhost")
              .execute().getContent must_== "1 condition met"
    }
  }

}

//
//  class MainRouter extends RequestRouter {
//    // Common stuff
//    get("/") = "preved"
//    get("/ctx") = if (!CircumflexContext.live_?) "null" else ctx.toString
//    get("/capture/(.*)"r) = "uri$1 is " + uri(1)
//    get("/decode me") = "preved"
//    post("/post") = "preved"
//    put("/put") = "preved"
//    delete("/delete") = "preved"
//    options("/options") = "preved"
//    get("/redirect") = redirect("/")
//    get("/rewrite") = rewrite("/")
//    get("/error") = error(503, "preved")
//    get("/contentType\\.(.+)"r) = {
//      ctx.contentType = uri.get(1) match {
//        case Some("html") => "text/html"
//        case Some("css") => "text/css"
//        case _ => "application/octet-stream"
//      }
//      done()
//    }
//    // Composite matchers
//    get("/composite" & Accept("text/:format") & Referer("localhost")) =
//        "3 conditions met (" + param("format") + ")"
//    get("/composite" & Accept("text/:format")) =
//        "2 conditions met (" + param("format") + ")"
//    get("/composite") = "1 condition met"
//    // Flashes
//    get("/flash-set") = {
//      flash('notice) = "preved"
//      done()
//    }
//    get("/flash-get") = flash.get('notice) match {
//      case Some(s: String) => s
//      case None => ""
//    }
//    // Simple urls
//    get("/filename/:name.:ext") = uri('name) + uri('ext)
//    get("*/one/:two/+.:three") =
//      uri(1) + uri('two) + uri(3) + uri('three)
//    // Messages
//    get("/msg") = msg("hello")
//    get("/msg/:name") = msg("parameterizedHello", "name" -> uri('name))
//  }
//
//  doBeforeSpec{
//    Circumflex("cx.router") = classOf[MainRouter]
//    MockApp.start
//  }
//
//  doAfterSpec { MockApp.stop }
//
//  "RequestRouter" should {
//    "match composite routes" in {
//      MockApp.get("/composite")
//          .setHeader("Accept","text/html")
//          .setHeader("Referer","localhost")
//          .execute().getContent must_== "3 conditions met (html)"
//      MockApp.get("/composite")
//          .setHeader("Accept","text/plain")
//          .execute().getContent must_== "2 conditions met (plain)"
//      MockApp.get("/composite")
//          .setHeader("Accept","application/xml")
//          .setHeader("Referer","localhost")
//          .execute().getContent must_== "1 condition met"
//    }
//    "send redirects" in {
//      val r = MockApp.get("/redirect").execute()
//      r.getStatus must_== 302
//      r.getHeader("Location") must_== "http://localhost/"
//    }
//    "process rewrites" in {
//      MockApp.get("/rewrite").execute().getContent must_== "preved"
//    }
//    "process errors" in {
//      MockApp.get("/error").execute().getStatus must_== 503
//    }
//  }
//
//  "UriMatcher" should {
//    "match simplified request 1" in {
//      MockApp.get("/filename/file.txt").execute.getContent must_== "filetxt"
//    }
//    "match simplified request 2" in {
//      MockApp.get("/aaa/one/bbb00/cc.ddd.e").execute.getContent must_== "/aaabbb00cc.ddde"
//      MockApp.get("/one/bbb00/cc.ddd.e").execute.getContent must_== "bbb00cc.ddde"
//      MockApp.get("/one/bbb00/.ddde").execute.getStatus must_== 404
//    }
//  }
//
//  }
//
