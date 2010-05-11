package ru.circumflex.core.test

import ru.circumflex.core._
import org.specs.runner.JUnit4
import org.specs.Specification

class SpecsTest extends JUnit4(CircumflexCoreSpec)

object CircumflexCoreSpec extends Specification {

  class SubRouterA extends RequestRouter {
    any("/sub1/*") = new SubRouterB
    any("/sub2/*") = new SubRouterB
    get("/testA") = rewrite("/sub2/testB")
  }
  class SubRouterB extends RequestRouter {
    get("/testB") = "preved"
  }

  class MainRouter extends RequestRouter {
    any("/sub/*") = new SubRouterA
    
    get("/") = "preved"
    get("/ctx") = if (!CircumflexContext.live_?) "null" else context.toString
    get("/capture/?"r, accept("+/+") & content_type("+/+")) =
        "Accept$1 is " + matching('Accept)(1) + "; " +
        "Accept$2 is " + matching('Accept)(2) + "; " +
        "Content$1 is " + matching("Content-Type")(1) + "; " +
        "Content$2 is " + matching("Content-Type")(2)
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
      context.contentType = uri(1) match {
        case "html" => "text/html"
        case "css" => "text/css"
        case _ => "application/octet-stream"
      }
      done()
    }
    get("/flash-set") = {
      flash('notice) = "preved"
      done()
    }
    get("/flash-get") = flash('notice) match {
      case Some(s: String) => s
      case None => ""
    }

    // Simple urls
    get("/filename/:name.:ext") = uri('name) + uri('ext)
    get("*/one/:two/+.:three") =
      uri(1) + uri('two) + uri(3) + uri('three)

    // Extractors
    get("/self/:name/:id", accept("+/+")) {
      case uri(name, Int(id)) & accept("text", what) =>
        "Name is " + name + "; 2*ID is " + (2 * id) + "; what is " + what
    }
    get("/host") {
      case host("localhost") => "local"
      case _                 => "remote"
    }
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
    "process URI extractors" in {
      MockApp.get("/self/abc/12")
             .setHeader("Accept", "text/plain")
             .execute.getContent must_== "Name is abc; 2*ID is 24; what is plain"
    }
    "process HOST extractors" in {
      MockApp.get("/host").execute.getContent must_== "local"
    }
    "process sub routers" in {
      MockApp.get("/testA").execute.getStatus must_== 404
      MockApp.get("/sub/testA").execute.getContent must_== "preved"
      MockApp.get("/sub/sub1/testB").execute.getContent must_== "preved"
    }
  }

  "UriMatcher" should {
    "match simplified request 1" in {
      MockApp.get("/filename/file.txt").execute.getContent must_== "filetxt"
    }
    "match simplified request 2" in {
      MockApp.get("/aaa/one/bbb00/cc.ddd.e").execute.getContent must_== "/aaabbb00ccddd.e"
      MockApp.get("/one/bbb00/cc.ddd.e").execute.getContent must_== "bbb00ccddd.e"
      MockApp.get("/one/bbb00/.ddde").execute.getStatus must_== 404
    }
  }

  "UriMatcher" should {
    "match simplified request 1" in {
      MockApp.get("/filename/file.txt").execute.getContent must_== "filetxt"
    }
    "match simplified request 2" in {
      MockApp.get("/aaa/one/bbb00/cc.ddd.e").execute.getContent must_== "/aaabbb00ccddd.e"
      MockApp.get("/one/bbb00/cc.ddd.e").execute.getContent must_== "bbb00ccddd.e"
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
    "contain captured groups from headers" in {
      MockApp.get("/capture")
          .setHeader("Accept", "text/plain")
          .setHeader("Content-Type", "text/html")
          .execute()
          .getContent must_== "Accept$1 is text; Accept$2 is plain; Content$1 is text; Content$2 is html"
    }
    "set response content type" in {
      MockApp.get("/contentType.html").execute()
          .getHeader("Content-Type") must beMatching("text/html(\\s*;\\s*charset=.*)?")
      MockApp.get("/contentType.css").execute()
          .getHeader("Content-Type") must beMatching("text/css(\\s*;\\s*charset=.*)?")
    }
  }

}
