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

import collection.mutable.HashMap
import java.io.File
import java.net.URLDecoder
import java.util.{Locale, ResourceBundle}
import javax.servlet.http.{HttpServletResponse, HttpServletRequest}
import javax.activation.MimetypesFileTypeMap
import org.slf4j.LoggerFactory
import org.apache.commons.io.FilenameUtils._

class CircumflexContext(val request: HttpServletRequest,
                        val response: HttpServletResponse,
                        val filter: AbstractCircumflexFilter)
    extends HashModel {

  val uri = URLDecoder.decode(request.getRequestURI, "UTF-8")
  val params = new HashMap[String, Any]

  val stringHeaders = new HashMap[String, String]
  val dateHeaders = new HashMap[String, Long]
  var statusCode: Int = 200
  protected var _contentType: String = null
  
  /* HELPERS */
  val header = new HeadersHelper
  val session = new SessionHelper
  val flash = new FlashHelper

  params += ("header" -> header)
  params += ("session" -> session)
  params += ("flash" -> flash)

  def contentType: Option[String] =
    if (_contentType == null) None
    else Some(_contentType)

  def contentType_=(value: String): Unit = {
    this._contentType = value;
  }

  def method: String =
    get("_method").getOrElse(request.getMethod).toString

  def get(key: String): Option[Any] = params.get(key) match {
    case Some(value) if (value != null) => Some(value)
    case _ => {
      val value = request.getParameter(key)
      if (value == null) None
      else Some(value)
    }
  }

  def stringParam(key: String): Option[String] = get(key) match {
    case Some(value) => Some(value.toString)
    case _ => None
  }

  def getOrElse[A](key: String, default: A): A = get(key) match {
    case Some(value: A) => value;
    case _ => default
  }

  def noCache() = {
    stringHeaders += "Pragma" -> "no-cache"
    stringHeaders += "Cache-Control" -> "no-store"
    dateHeaders += "Expires" -> 0l
  }

  def +=(pair: Pair[String, Any]): Unit = params += pair

  def ++=(map: Map[String, Any]): Unit = params ++= map

  def apply(key: String): Option[Any] = get(key)

}


object Circumflex {

  val log = LoggerFactory.getLogger("ru.circumflex.core")

  /* CONFIGURATION */

  private val _cfg = new HashMap[String, Any]
  val cfg = new ConfigurationHelper

  /* CONFIGURATION DEFAULTS */

  // should filter process request?
  val f: HttpServletRequest => Boolean = r => !r.getRequestURI.toLowerCase.matches("/public/.*")
  _cfg += "cx.process_?" -> f
  // webapp root
  _cfg += "cx.root" -> "src/main/webapp"
  // static files directory (relative to webapp root)
  _cfg += "cx.public" -> "public"
  // resource bundle for messages
  _cfg += "cx.messages" -> "Messages"

  val classLoader: ClassLoader = cfg("cx.classLoader") match {
    case Some(cld: ClassLoader) => cld
    case _ => Thread.currentThread.getContextClassLoader
  }

  val webappRoot: File = cfg("cx.root") match {
    case Some(s: String) => new File(separatorsToSystem(s))
    case _ => throw new CircumflexException("'cx.root' not configured.")
  }

  val publicRoot = cfg("cx.public") match {
    case Some(s: String) => new File(webappRoot, separatorsToSystem(s))
    case _ => throw new CircumflexException("'cx.public' not configured.")
  }

  val XSendFileHeader = cfg("cx.xSendFile.header") match {
    case Some(s: String) => s
    case _ => "X-SendFile"
  }

  try {     // read configuration from "cx.properties" by default
    val bundle = ResourceBundle.getBundle("cx", Locale.getDefault, classLoader)
    val keys = bundle.getKeys
    while (keys.hasMoreElements) {
      val k = keys.nextElement
      cfg(k) = bundle.getString(k)
    }
  } catch {
    case _ => log.warn("Could not read configuration parameters from cx.properties.")
  }

  /**
   * For DSL-like configuration
   */
  class ConfigurationHelper {
    def apply(key: String): Option[Any] = _cfg.get(key)
    def apply(key: String, default: Any): Any = _cfg.getOrElse(key, default)
    def update(key: String, value: Any): Unit =
      _cfg += key -> value
  }

  /* MESSAGES */

  def msg(locale: Locale): Messages = cfg("cx.messages") match {
    case Some(s: String) => new Messages(s, locale)
    case _ => throw new CircumflexException("'cx.messages' not configured.")
  }

  /* CONTEXT */

  private val threadLocalContext = new ThreadLocal[CircumflexContext]

  def ctx = threadLocalContext.get

  def initContext(req: HttpServletRequest,
                  res: HttpServletResponse,
                  filter: AbstractCircumflexFilter) = {
    threadLocalContext.set(new CircumflexContext(req, res, filter))
    try {
      ctx += "msg" -> msg(req.getLocale)
    } catch {
      case e => log.debug("Could not instantiate context messages.", e)
    }
  }

  def destroyContext() = threadLocalContext.set(null)

  /* MISCELLANEOUS */

  def mimeTypesMap = new MimetypesFileTypeMap()

}

class CircumflexException(msg: String, cause: Throwable)
    extends Exception(msg, cause) {
  def this(msg: String) = this(msg, null)
  def this(cause: Throwable) = this(null, cause)
}
