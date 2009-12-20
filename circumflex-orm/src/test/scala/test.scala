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

class Category extends Record[Category] {
  def relation = Category
  val id = field(Category.id)
  val name = field(Category.name)
  val books = oneToMany(Book.category)
}

object Category extends GenericTable[Category] {
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

  def sqlCreate = "CREATE TRIGGER category_trig\n" +
          "AFTER INSERT OR UPDATE ON orm.category\n" +
          "FOR EACH ROW EXECUTE PROCEDURE orm.category_trig_func()"

  def sqlDrop = "DROP TRIGGER category_trig ON orm.category"

}

class Book extends Record[Book] {
  def relation = Book
  val id = field(Book.id)
  val title = field(Book.title)
  val category = manyToOne(Book.category)
}

object Book extends GenericTable[Book] {
  val title = stringColumn("title")
          .notNull
          .validateNotEmpty
  val category = longColumn("category_id")
          .references(Category)     // Creates an association with Category
          .onDeleteSetNull
          .onUpdateCascade
}

class CategoryStatistics extends Record[CategoryStatistics] {
  def relation = CategoryStatistics
  val category = manyToOne(CategoryStatistics.category)
  val booksCount = field(CategoryStatistics.booksCount)
}

object CategoryStatistics extends View[CategoryStatistics] {
  import Query._

  val category = column[Long]("category_id")
          .references(Category)

  val booksCount = column[Long]("books_count")

  def query = select("c.id", count("b.id"))
          .from(Category as "c" join (Book as "b"))

  def primaryKey = pk(category.localColumn)

}

class BookWithCategory extends Record[BookWithCategory] {
  def relation = BookWithCategory
  val book = proxy(BookWithCategory.book)
  val category = proxy(BookWithCategory.category)
}

object BookWithCategory extends View[BookWithCategory] {
  import Query._

  val b = Book as "b"
  val c = Category as "c"

  val book = inlineRecord(b)
  val category = inlineRecord(c)

  def query = select(b.*, c.*).from(b join c)

  def primaryKey = pk(book.pkColumn)
}
