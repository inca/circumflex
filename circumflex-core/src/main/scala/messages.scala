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

import java.util.{Locale, MissingResourceException, ResourceBundle}
import org.slf4j.LoggerFactory

class Messages(val baseName: String, val locale: Locale) extends HashModel {

  val log = LoggerFactory.getLogger("ru.circumflex.core")

  val msgBundle: ResourceBundle = try {
    ResourceBundle.getBundle(baseName, locale)
  } catch {
    case e => {
      log.trace("ResourceBundle for messages instance not found: " + baseName)
      null
    }
  }

  def apply(key: String): Option[String] = try {
    Some(msgBundle.getString(key))
  } catch {
    case e => None
  }

  def apply(key: String, params: collection.Map[String, String]): Option[String] =
    apply(key) match {
      case Some(msg) => Some(params.foldLeft(msg) {
        (m, p) => m.replaceAll("\\{" + p._1 + "\\}", p._2)
      })
      case None => None
    }

  def get(key: String) = apply(key) match {
    case None =>
      log.warn("Missing message for key {}, locale {}.", key, msgBundle.getLocale)
      Some("")
    case v => v
  }

  def get(key: String, params: collection.Map[String, String]) =
    apply(key, params) match {
      case None =>
        log.warn("Missing message for key {}, locale {}.", key, msgBundle.getLocale)
        Some("")
      case v => v
    }
}

object Messages {
  def apply(): Messages = {
    if (Circumflex.ctx == null)
      throw new CircumflexException("CircumflexContext is not available.")
    Circumflex.ctx.get("msg") match {
      case Some(m: Messages) => m
      case _ => throw new CircumflexException("Messages instance not found in CircumflexContext.")
    }
  }
}
