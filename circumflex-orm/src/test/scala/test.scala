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

import ORM._

class Category extends Record[Category] {
  val id = field(Category.id)
  val name = field(Category.name)
  val books = oneToMany(Book.category)
}

object Category extends Table[Category] with LongIdPK[Category] {
  val namePattern = "^[a-zA-Z]+$"
  val name = stringColumn("name")             // Creates a column
          .notNull                            // Creates NOT NULL constraint
          .unique                             // Creates UNIQUE constraint
          .validateNotEmpty                   // Adds NotEmpty validation
          .validatePattern(namePattern)       // Adds Pattern validation
  check("name ~ '" + namePattern +"'")
  addAuxiliaryObjects(CategoryTriggerFunction, CategoryTrigger)
}

object CategoryTriggerFunction extends SchemaObject {
  def objectName = "orm.category_trig_func"
  def sqlCreate = """
  CREATE FUNCTION orm.category_trig_func() RETURNS TRIGGER AS $BODY$
  BEGIN
    RAISE NOTICE 'PREVED!!!11';
    RETURN NEW;
  END;
  $BODY$ LANGUAGE 'plpgsql'"""
  def sqlDrop = "DROP FUNCTION orm.category_trig_func()"
}

object CategoryTrigger extends SchemaObject {
  def objectName = "category_trig"
  def sqlCreate = "CREATE TRIGGER category_trig " +
          "AFTER INSERT OR UPDATE ON orm.category " +
          "FOR EACH ROW EXECUTE PROCEDURE orm.category_trig_func()"
  def sqlDrop = "DROP TRIGGER category_trig ON orm.category"
}

class Book extends Record[Book] {
  val id = field(Book.id)
  val title = field(Book.title)
  val category = manyToOne(Book.category)
}

object Book extends Table[Book] with LongIdPK[Book] {
  val title = stringColumn("title")
          .notNull
          .validateNotEmpty
  val category = longColumn("category_id")
          .references(Category)     // Creates an association with Category
          .onDeleteSetNull
          .onUpdateCascade
  index.unique.add("lower(title)").where("title" like "A%")
}

class Page extends Record[Page] {
  val id = field(Page.id)
  val body = field(Page.body)
  val book = manyToOne(Page.book)
}

object Page extends Table[Page] with LongIdPK[Page] {
  val book = longColumn("book_id")
          .notNull
          .references(Book)
          .onDeleteCascade
  val body = stringColumn("body")
          .notNull
          .validateNotEmpty
}

class CategoryStatistics extends Record[CategoryStatistics] {
  val category = manyToOne(CategoryStatistics.category)
  val booksCount = field(CategoryStatistics.booksCount)
}

object CategoryStatistics extends View[CategoryStatistics] {
  val category = virtualColumn[Long]("category_id")
          .references(Category)
  val booksCount = virtualColumn[Long]("books_count")
  pk(category.localColumn)
  def query = select("c.id", count("b.id"))
          .from(Category as "c" join (Book as "b"))
}
