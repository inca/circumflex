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

import javax.servlet.http._
import scala.collection.mutable.HashMap
import Circumflex._

/**
 * A helper for setting flashes. Flashes provide a way to pass temporary objects between requests.
 */
class FlashHelper extends HashModel {

  private val _key = "cx.flash"

  def apply(key: String): Option[Any] = {
    val flashMap = ctx.request.getSession.getAttribute(_key) match {
      case m: HashMap[String, Any] => m
      case _ => new HashMap[String, Any]()
    }
    flashMap.get(key) match {
      case Some(value) =>
        ctx.request.getSession.setAttribute(_key, flashMap - key)
        return Some(value)
      case _ => return None
    }
      
  }

  def update(key: String, value: Any): Unit = {
    val flashMap = ctx.request.getSession.getAttribute(_key) match {
      case m: HashMap[String, Any] => m
      case _ => new HashMap[String, Any]()
    }
    ctx.request.getSession.setAttribute(_key, flashMap + (key -> value))
  }

  def get(key: String) = apply(key)

}
