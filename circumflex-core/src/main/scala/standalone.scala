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

package ru.circumflex.core
import javax.servlet.Filter
import org.mortbay.jetty.servlet.{ServletHolder, DefaultServlet, Context}
import org.mortbay.jetty.{Handler, Server}

/**
 * A helper that allows standalone Circumflex execution based on
 * Jetty server.
 */
trait StandaloneServer {

  def filters: Seq[Class[_ <: Filter]] = List(classOf[CircumflexFilter])

  protected var jetty: Server = null
  protected var context: Context = null

  def init() = {
    jetty = new Server(Circumflex.cfg("cx.port") match {
      case Some(p: Int) => p
      case Some(s: String) => try { s.toInt } catch { case _ => 8181 }
      case _ => 8181
    })
    context = new Context(jetty, "/", Context.SESSIONS)
    context.setResourceBase(Circumflex.cfg("cx.root") match {
      case Some(s: String) => s
      case _ => "src/main/webapp"
    })
    context.addServlet(new ServletHolder(new DefaultServlet), "/*")
    filters.foreach(f => context.addFilter(f, "/*", Handler.ALL))
  }

  def start = {
    init()
    jetty.start
  }

  def stop = if (jetty != null) jetty.stop

}