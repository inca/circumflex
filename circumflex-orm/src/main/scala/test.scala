package ru.circumflex.orm

class Category extends Record {

}

object Category extends GenericTable[Category] {
  val name = stringColumn("name")       // Creates a column
      .notNull                          // Creates NOT NULL constraint
      .unique                           // Creates UNIQUE constraint
}

class Book extends Record {

}

object Book extends GenericTable[Book] {
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

object Page extends GenericTable[Page] {
  val book = longColumn("book_id")
      .notNull
      .references(Book)
      .onDeleteCascade
      .onUpdateCascade
  val body = stringColumn("body")
}


object Test extends Application {

  val c = Category as "c"
  val b = Book as "b"
  val p = Page as "p"
  val b1 = Book as "p1"

  val j = c.join(b.join(p)).join(b1)

  println(j.toSql)

}
