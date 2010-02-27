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

package ru.circumflex.orm.i18n

import ru.circumflex.orm._
import ru.circumflex.core._
import java.lang.String
import java.util.Locale
import ORM._
import ORMI18N._
import java.sql.{PreparedStatement, Connection}

class LocalizedTransaction extends StatefulTransaction
        with JDBCHelper{
  protected var _currentLanguage: String = null

  def setLanguage(lang: String): this.type = {
    _currentLanguage = lang
    val q = i18nDialect.setLangQuery(_currentLanguage)
    dml (conn => auto(conn.prepareStatement(q))(st => {
      sqlLog.debug(q)
      st.executeUpdate
    }))
    return this
  }
  def setLanguage(locale: Locale): this.type = setLanguage(locale.getLanguage)

  def currentLanguage: Option[String] =
    if (_currentLanguage == null) None
    else Some(_currentLanguage)

  def languageSet_?(): Boolean = _currentLanguage != null

  override def sql[A](sql: String)(actions: PreparedStatement => A): A = {
    if (!languageSet_?) setLanguage(ORMI18N.defaultLanguage)
    super.sql(sql)(actions)
  }
  
  override def dml[A](actions: Connection => A): A = {
    if (!languageSet_?) setLanguage(ORMI18N.defaultLanguage)
    super.dml(actions)
  }
}

class I18nTransactionManager extends TransactionManager {
  def openLocalizedTransaction: LocalizedTransaction = new LocalizedTransaction()
  def getLocalizedTransaction = getTransaction.asInstanceOf[LocalizedTransaction]
  override def openTransaction() = openLocalizedTransaction
}

/**
 * ORM Internationalization features configuration.
 */
object ORMI18N {
  val langSetting = Circumflex.cfg("orm.i18n.langSetting") match {
    case Some(s: String) => s
    case _ => "cx.lang"
  }
  val defaultLanguage = Circumflex.cfg("orm.i18n.defaultLanguage") match {
    case Some(l: String) => l.trim.toLowerCase
    case _ => "en"
  }
  val i18nDialect: I18NDialect = ORM.dialect match {
    case d: I18NDialect => d
    case _ => throw new ORMException("'orm.dialect' must point to valid I18NDialect.")
  }
  val i18nTransactionManager = ORM.transactionManager match {
    case tm: I18nTransactionManager => tm
    case _ => throw new ORMException("'orm.transactionManager' must point to valid I18nTransactionManager.")
  }
  def ltx: LocalizedTransaction = i18nTransactionManager.getLocalizedTransaction
}