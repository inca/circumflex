package ru.circumflex
package web

import core._
import matchers._
import org.specs.runner.JUnit4
import org.specs.Specification
import java.io.{File}

class SpecsTest extends JUnit4(CircumflexWebSpec)

class MockRouter extends Router {
  get("/") = "preved"
  post("/") = "preved from POST"
  get("/decode me") = "preved"
  get("/regex/(.*)".r) = "matched " + uri(1)
  put("/") = "this is a put route"

  get("/error") = error(503)
  get("/redirect") = redirect("/")

  sub("/sub") = new SubMockRouter

  var counter = 0

  filter("/filter") = {
    counter += 1
  }

  get("/filter") = counter.toString

  sub("/matching") = new MatchingMockRouter
}

class SubMockRouter extends Router {
  get("/?") = "preved"
}

class MatchingMockRouter extends Router {
  get("/uri/:name.:ext") = uri("name") + "->" + uri("ext")
  get("/uri/:name") = "preved, " + uri("name")
  get("/uri/*/one/:two/+.:three") = uri(1) + uri("two") + uri(3) + uri("three")

  get("/param").and(HOST(":host")) = "host is " + param("host")

  get("/composite").and(ACCEPT("text/:format")).and(REFERER("localhost")) =
      "3 conditions met (" + param("format") + ")"
  get("/composite").and(ACCEPT("text/:format")) =
      "2 conditions met (" + param("format") + ")"
  get("/composite") = "1 condition met"

  get("/multiparam") = request.params.list("test").mkString(",")
  get("/multiparam/:test/:test") = param.list("test").mkString(",")

  get("/complex/:name")
      .and(param("name").startsWith("Ch")) = "You passed a complex route."

  get("/complex/:name")
      .and(false)
      .and({
    println("Unreachable code.")
    true
  }) = "You can't be there."

  get("/complex/:name") = "You failed to pass complex route using '" + param("name") + "'."
}

object CircumflexWebSpec extends Specification {

  doBeforeSpec{
    cx("cx.router") = classOf[MockRouter]
    var rootPath = System.getProperty("user.dir")
    if (!rootPath.endsWith("circumflex-web")) {
      rootPath += (File.separator + "circumflex-web")
    }
    cx("cx.webappRoot") = (rootPath + File.separatorChar + "src/test/webapp")
    MockApp.start()
  }

  doAfterSpec { MockApp.stop() }

  "Router" should {
    "return 404 by default on non-matched requests" in {
      MockApp.get("/this/does/not/match/any/routes").execute().statusCode must_== 404
    }
    "decode URIs before matching" in {
      MockApp.get("/decode%20me").execute().content must_== "preved"
    }
    "match requests by simple URI" in {
      MockApp.get("/").execute().content must_== "preved"
    }
    "match requests by method" in {
      MockApp.post("/").execute().content must_== "preved from POST"
    }
    "match requests by regex" in {
      MockApp.get("/regex/piu").execute().content must_== "matched piu"
    }
    "interpret `_method` parameter as HTTP method" in {
      MockApp.get("/?_method=PUT").execute().content must_== "this is a put route"
      MockApp.post("/")
          .setParam("_method", "PUT")
          .execute()
          .content must_== "this is a put route"
    }
    "process subrouters" in {
      MockApp.get("/sub/").execute().content must_== "preved"
    }
    "send errors" in {
      MockApp.get("/error").execute().statusCode must_== 503
    }
    "send redirects" in {
      MockApp.get("/redirect").execute().content must_== "preved"
    }
    "process filter directive" in {
      MockApp.get("/filter").execute().content must_== "1"
    }
  }

  "Matching mechanism" should {
    "process named parameters from URI" in {
      MockApp.get("/matching/uri/Jack").execute().content must_== "preved, Jack"
      MockApp.get("/matching/uri/file.txt").execute().content must_== "file->txt"
      MockApp.get("/matching/uri/I/one/Love/Circum.flex").execute().content must_== "ILoveCircumflex"
    }
    "process named parameters from current match results, delegating to request parameters on fail" in {
      MockApp.get("/matching/param")
          .setHeader("Host", "preved")
          .execute()
          .content must_== "host is preved"
    }
    "match composite routes" in {
      MockApp.get("/matching/composite")
          .setHeader("Accept","text/html")
          .setHeader("Referer","localhost")
          .execute().content must_== "3 conditions met (html)"
      MockApp.get("/matching/composite")
          .setHeader("Accept","text/plain")
          .execute().content must_== "2 conditions met (plain)"
      MockApp.get("/matching/composite")
          .setHeader("Accept","application/xml")
          .setHeader("Referer","localhost")
          .execute().content must_== "1 condition met"
    }
    "deal with multiple parameter values" in {
      MockApp.get("/matching/multiparam?test=one&test=two&test=three")
          .execute()
          .content must_== "one,two,three"
      MockApp.get("/matching/multiparam/one/two?test=three&test=four&test=five")
          .execute()
          .content must_== "one,two,three,four,five"
    }
    "deal with complex route contexts" in {
      MockApp.get("/matching/complex/Chris")
          .execute()
          .content must_== "You passed a complex route."
      MockApp.get("/matching/complex/Chuck")
          .execute()
          .content must_== "You passed a complex route."
      MockApp.get("/matching/complex/Joe")
          .execute()
          .content must_== "You failed to pass complex route using 'Joe'."
    }
  }

}
