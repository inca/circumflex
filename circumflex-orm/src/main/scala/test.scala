package ru.circumflex.orm

class Category extends Record {

}

object Category extends GenericTable[Category]("public", "category") {
  val name = stringColumn("name").notNull.unique
}

class Book extends Record {

}

object Book extends GenericTable[Book]("public", "book") {
  val title = stringColumn("title").notNull
  val category = longColumn("category_id").notNull
      .references(Category)
      .onDeleteRestrict
      .onUpdateCascade
}



object Main extends Application {

  println(Category.sqlCreate)
  println(Book.sqlCreate)
  Category.sqlCreateConstraints.foreach(println(_))
  Book.sqlCreateConstraints.foreach(println(_))
  Book.sqlDropConstraints.foreach(println(_))
  Category.sqlDropConstraints.foreach(println(_))
  println(Book.sqlDrop)
  println(Category.sqlDrop)

}