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

import javax.servlet.http.Cookie
import org.mortbay.jetty.Handler
import org.mortbay.jetty.servlet.{DefaultServlet}
import org.mortbay.jetty.testing.{HttpTester, ServletTester}

trait MockServer extends StandaloneServer {

  val tester = new ServletTester()
  tester.setContextPath("/")
  tester.setResourceBase(webappRoot)
  tester.addServlet(classOf[DefaultServlet], "/*")
  filters.foreach(f => tester.addFilter(f, "/*", Handler.ALL))

  override def start = tester.start
  override def stop = tester.stop

  /* Methods */

  def get(uri: String) = new MockRequest(this, "GET", uri)

  def post(uri: String) = new MockRequest(this, "POST", uri)

  def put(uri: String) = new MockRequest(this, "PUT", uri)

  def delete(uri: String) = new MockRequest(this, "DELETE", uri)

  def head(uri: String) = new MockRequest(this, "HEAD", uri)

  def options(uri: String) = new MockRequest(this, "OPTIONS", uri)

}

class MockRequest(val mockServer: MockServer, val method: String, val uri: String) {

  private val req = new HttpTester

  req.setMethod(method)
  req.setURI(uri)
  req.setVersion("HTTP/1.1")
  req.setHeader("Host", "localhost")

  def setHeader(name: String, value: String): this.type = {
    req.setHeader(name, value)
    return this
  }

  def setDateHeader(name: String, value: Long): this.type = {
    req.setDateHeader(name, value)
    return this
  }

  def setLongHeader(name: String, value: Long): this.type = {
    req.setLongHeader(name, value)
    return this
  }

  def setContent(content: String): this.type = {
    req.setContent(content)
    return this
  }

  def setCookie(cookie: Cookie): this.type = {
    req.addSetCookie(cookie)
    return this
  }

  override def toString = req.generate

  def execute(): HttpTester = {
    val result = new HttpTester
    result.parse(mockServer.tester.getResponses(req.generate))
    return result
  }

}

object MockApp extends MockServer