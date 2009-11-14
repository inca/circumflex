package ru.circumflex.orm

class Category extends Record {
  def relation = Category
  val id = field(Category.id)
  val name = field(Category.name)

  override def toString =
    if (isIdentified) id.toString + ": " + name.toString
    else "UNKNOWN: " + name.toString
}

object Category extends Table[Category] {
  def recordClass = classOf[Category]
  def primaryKey = pk(id)
  val id = longColumn("id").notNull
  val name = stringColumn("name")       // Creates a column
      .notNull                          // Creates NOT NULL constraint
      .unique                           // Creates UNIQUE constraint
}

class Book extends Record {
  def relation = Book
  val id = field(Book.id)
  val title = field(Book.title)

  override def toString =
    if (isIdentified) id.toString + ": " + title.toString
    else "UNKNOWN: " + title.toString
}

object Book extends Table[Book] {
  def recordClass = classOf[Book]
  def primaryKey = pk(id)
  val id = longColumn("id").notNull
  val title = stringColumn("title")
      .notNull
  val category = longColumn("category_id")
      .references(Category)     // Creates an association with Category
      .onDeleteSetNull
      .onUpdateCascade
}
