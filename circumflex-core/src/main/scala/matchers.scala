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

import javax.servlet.http.HttpServletRequest
import util.matching.Regex


trait RequestMatcher {

  def extractMatches(regex: Regex,
                     src: String,
                     groupPrefix: String): Option[Map[String, String]] = {
    val m = regex.pattern.matcher(src)
    if (m.matches) {
      var matches = Map[String, String]()
      (1 to m.groupCount).foreach(ix => {
        matches += groupPrefix + ix -> m.group(ix)
      })
      Some(matches)
    } else None
  }

  def apply(request: HttpServletRequest): Option[Map[String, String]]

}

case class UriRegexMatcher(val uriRegex: String) extends RequestMatcher {

  def apply(request: HttpServletRequest) =
    extractMatches(uriRegex.r, request.getRequestURI, "uri$")

}

case class HeadersRegexMatcher(val criteria: (String, String)*) extends RequestMatcher {

  def apply(request: HttpServletRequest): Option[Map[String, String]] = {
    var params = Map[String,String]()
    criteria.toList.foreach(
      crit => matchHeader(crit._1, request.getHeader(crit._1), crit._2.r) match {
        case Some(p) => params ++= p
        case _ => return None
      })
    Some(params)
  }

  def matchHeader(headerName: String, headerValue: String, crit: Regex): Option[Map[String, String]] =
    if (headerValue == null) None
    else extractMatches(crit, headerValue, headerName + "$")

}
