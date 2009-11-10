package ru.circumflex.orm

class Category extends Record {
  val id = field(Category.id)
  val name = field(Category.name)
}

object Category extends Table[Category] {
  def primaryKey = pk(id)
  val id = longColumn("id").notNull
  val name = stringColumn("name")       // Creates a column
      .notNull                          // Creates NOT NULL constraint
      .unique                           // Creates UNIQUE constraint
}

class Book extends Record {
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
