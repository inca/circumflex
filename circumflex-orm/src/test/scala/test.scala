package ru.circumflex.orm

class Category extends Record {
  val id = Category.id()
  val name = Category.name()
}

object Category extends Table[Category] {
  def primaryKey = pk(id)
  val id = longColumn("id").notNull
  val name = stringColumn("name")       // Creates a column
      .notNull                          // Creates NOT NULL constraint
      .unique                           // Creates UNIQUE constraint
}

class Book extends Record {
  val id = Book.id()
  val name = Book.title()
  val category = Book.category()
}

object Book extends Table[Book] {
  def primaryKey = pk(id)
  val id = longColumn("id").notNull
  val title = stringColumn("title")
      .notNull
  val category = longColumn("category_id")
      .notNull
      .references(Category)     // Creates an association with Category
      .onDeleteRestrict
      .onUpdateCascade
}

class Page extends Record {
  val id = Page.id()
  val book = Page.book()
  val body = Page.body()
}

object Page extends Table[Page] {
  def primaryKey = pk(id)
  val id = longColumn("id").notNull
  val book = longColumn("book_id")
      .notNull
      .references(Book)
      .onDeleteCascade
      .onUpdateCascade
  val body = stringColumn("body")
}
