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
  get("/uri/:name.:ext") = uri('name) + "->" + uri('ext)
  get("/uri/:name") = "preved, " + uri('name)
  get("/uri/*/one/:two/+.:three") = uri(1) + uri('two) + uri(3) + uri('three)

  get("/param" & HOST(":host")) = "host is " + param("host")

  get("/composite" & ACCEPT("text/:format") & REFERER("localhost")) =
          "3 conditions met (" + param("format") + ")"
  get("/composite" & ACCEPT("text/:format")) =
          "2 conditions met (" + param("format") + ")"
  get("/composite") = "1 condition met"

  get("/multiparam") = request.params.list("test").mkString(",")
  get("/multiparam/:test/:test") = param.list("test").mkString(",")
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
    "deal with multiple parameter values" in {
      MockApp.get("/matching/multiparam?test=one&test=two&test=three")
          .execute()
          .getContent must_== "one,two,three"
      MockApp.get("/matching/multiparam/one/two?test=three&test=four&test=five")
          .execute()
          .getContent must_== "one,two,three,four,five"
    }
  }

}
