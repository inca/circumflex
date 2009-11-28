package ru.circumflex.orm

class Category extends Record[Category] {
  def relation = Category
  val id = field(Category.id)
  val name = field(Category.name)
}

object Category extends GenericTable[Category] {
  val name = stringColumn("name")         // Creates a column
      .notNull                            // Creates NOT NULL constraint
      .unique                             // Creates UNIQUE constraint
      .validateNotEmpty                   // Adds NotEmpty validation
      .validatePattern("^[a-zA-Z]{1,8}$") // Adds Pattern validation
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
