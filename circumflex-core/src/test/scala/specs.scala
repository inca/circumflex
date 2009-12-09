/*
 * Copyright (C) 2009-2010 Boris Okunskiy (http://incarnate.ru)
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

package ru.circumflex.core.test

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
    
  }

}