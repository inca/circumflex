package ru.circumflex.orm

class Category extends Record[Category] {
  def relation = Category
  val name = field(Category.name)
}

object Category extends GenericTable[Category] {
  val name = stringColumn("name")       // Creates a column
      .notNull                          // Creates NOT NULL constraint
      .unique                           // Creates UNIQUE constraint
}

class Book extends Record[Book] {
  def relation = Book
  val title = field(Book.title)
  val category = manyToOne(Book.category)
}

object Book extends GenericTable[Book] {
  val title = stringColumn("title")
      .notNull
  val category = longColumn("category_id")
      .references(Category)     // Creates an association with Category
      .onDeleteSetNull
      .onUpdateCascade
}
