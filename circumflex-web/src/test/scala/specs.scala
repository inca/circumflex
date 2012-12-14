package pro.savant.circumflex
package web

import core._
import matchers._
import java.io.File
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.MustMatchers
import org.scalatest._

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

@RunWith(classOf[JUnitRunner])
class CircumflexWebSpec
  extends FreeSpec
  with MustMatchers
  with BeforeAndAfter {

  before {
    cx("cx.router") = classOf[MockRouter]
    var rootPath = System.getProperty("user.dir")
    if (!rootPath.endsWith("circumflex-web")) {
      rootPath += (File.separator + "circumflex-web")
    }
    cx("cx.webappRoot") = (rootPath + File.separatorChar + "src/test/webapp")
    MockApp.start()
  }

  after { MockApp.stop() }

  "Router" - {
    "return 404 by default on non-matched requests" in {
      MockApp.get("/this/does/not/match/any/routes").execute().statusCode must equal (404)
    }
    "decode URIs before matching" in {
      MockApp.get("/decode%20me").execute().content must equal ("preved")
    }
    "match requests by simple URI" in {
      MockApp.get("/").execute().content must equal("preved")
    }
    "match requests by method" in {
      MockApp.post("/").execute().content must equal("preved from POST")
    }
    "match requests by regex" in {
      MockApp.get("/regex/piu").execute().content must equal ("matched piu")
    }
    "interpret `_method` parameter as HTTP method" in {
      MockApp.get("/?_method=PUT").execute().content must equal ("this is a put route")
      MockApp.post("/")
          .setParam("_method", "PUT")
          .execute()
          .content must equal ("this is a put route")
    }
    "process subrouters" in {
      MockApp.get("/sub/").execute().content must equal ("preved")
    }
    "send errors" in {
      MockApp.get("/error").execute().statusCode must equal (503)
    }
    "send redirects" in {
      MockApp.get("/redirect").execute().content must equal ("preved")
    }
    "process filter directive" in {
      MockApp.get("/filter").execute().content must equal ("1")
    }
  }

  "Matching mechanism" - {
    "process named parameters from URI" in {
      MockApp.get("/matching/uri/Jack").execute().content must equal ("preved, Jack")
      MockApp.get("/matching/uri/file.txt").execute().content must equal ("file->txt")
      MockApp.get("/matching/uri/I/one/Love/Circum.flex").execute().content must equal ("ILoveCircumflex")
    }
    "process named parameters from current match results, delegating to request parameters on fail" in {
      MockApp.get("/matching/param")
          .setHeader("Host", "preved")
          .execute()
          .content must equal ("host is preved")
    }
    "match composite routes" in {
      MockApp.get("/matching/composite")
          .setHeader("Accept","text/html")
          .setHeader("Referer","localhost")
          .execute().content must equal ("3 conditions met (html)")
      MockApp.get("/matching/composite")
          .setHeader("Accept","text/plain")
          .execute().content must equal ("2 conditions met (plain)")
      MockApp.get("/matching/composite")
          .setHeader("Accept","application/xml")
          .setHeader("Referer","localhost")
          .execute().content must equal ("1 condition met")
    }
    "deal with multiple parameter values" in {
      MockApp.get("/matching/multiparam?test=one&test=two&test=three")
          .execute()
          .content must equal ("one,two,three")
      MockApp.get("/matching/multiparam/one/two?test=three&test=four&test=five")
          .execute()
          .content must equal ("one,two,three,four,five")
    }
    "deal with complex route contexts" in {
      MockApp.get("/matching/complex/Chris")
          .execute()
          .content must equal ("You passed a complex route.")
      MockApp.get("/matching/complex/Chuck")
          .execute()
          .content must equal ("You passed a complex route.")
      MockApp.get("/matching/complex/Joe")
          .execute()
          .content must equal ("You failed to pass complex route using 'Joe'.")
    }
  }

}
