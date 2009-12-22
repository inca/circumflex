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

package ru.circumflex.orm

import ru.circumflex.core.Circumflex

/**
 * Aggregates all ORM-related interfaces into configuration object.
 * You may want to provide your own implementation of all these methods
 * if you are not satisfied with default ones.
 */
object ORM {
  /**
   * Returns connection provider.
   * Can be overriden with "orm.connectionProvider" configuration parameter.
   */
  val connectionProvider: ConnectionProvider = Circumflex.cfg("orm.connectionProvider") match {
    case Some(p: ConnectionProvider) => p
    case Some(c: Class[ConnectionProvider]) => c.newInstance
    case Some(s: String) => Class.forName(s).asInstanceOf[Class[ConnectionProvider]].newInstance
    case _ => DefaultConnectionProvider
  }

  /**
   * Returns SQL dialect.
   * Can be overriden with "orm.dialect" configuration parameter.
   */
  val dialect: Dialect = Circumflex.cfg("orm.dialect") match {
    case Some(d: Dialect) => d
    case Some(c: Class[Dialect]) => c.newInstance
    case Some(s: String) => Class.forName(s).asInstanceOf[Class[Dialect]].newInstance
    case _ => DefaultDialect
  }

  /**
   * Returns SQL type converter.
   * Can be overriden with "orm.typeConverter" configuration parameter.
   */
  val typeConverter: TypeConverter = Circumflex.cfg("orm.typeConverter") match {
    case Some(tc: TypeConverter) => tc
    case Some(c: Class[TypeConverter]) => c.newInstance
    case Some(s: String) => Class.forName(s).asInstanceOf[Class[TypeConverter]].newInstance
    case _ => DefaultTypeConverter
  }

  val defaultSchemaName = Circumflex.cfg("orm.defaultSchema") match {
    case Some(s: String) => s
    case _ => "public"
  }
}