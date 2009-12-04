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

package ru.circumflex.site

import core.RequestRouter
import freemarker.FreemarkerHelper
import java.text.SimpleDateFormat
import java.util.Date

class Main extends RequestRouter
    with FreemarkerHelper {

  ctx += "currentYear" -> new SimpleDateFormat("yyyy").format(new Date)
  ctx += "host" -> header("Host").getOrElse("localhost")

  get("/") = ftl("index.ftl")
  get("/start") = ftl("start.ftl")
  get("/core") = ftl("core.ftl")
  get("/orm") = ftl("orm.ftl")
  get("/guide") = ftl("guide.ftl")

  // Guide examples

  get("/guide/hello/(.+)") = "Saying hello to " + ctx("uri$1").get
  get("/guide/hello", headers("Host" -> "(.*)", "User-Agent" -> "(.*)")) =
      "Host is " + ctx("Host$1").get + "; user agent is " + ctx("User-Agent$1").get

}