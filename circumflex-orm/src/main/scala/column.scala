package ru.circumflex.orm

/**
 * Represents an abstract backend-specific column.
 */
abstract class Column[T](val table: Table,
                         val name: String) {

  /**
   * Specifies SQL type of this column.
   * @return column SQL type (e.q. "VARCHAR" or "INT8"); case is ignored.
   */
  def sqlType: String

  /**
   * Specifies whether NOT NULL constraint is applied to this column.
   * @return <code>false</code> if NOT NULL constraint is applied;
   *         <code>true</code> otherwise
   */
  def nullable: Boolean = false;

  /**
   * Specifies whether UNIQUE constraint is applied to this column.
   * @return <code>false</code> if UNIQUE constraint is applied;
   *         <code>true</code> otherwise
   */
  def unique: Boolean = false;

  /**
   * Produces SQL definition for a column (e.q. "mycolumn varchar not null unique")
   * for schema generation purposes using Dialect object provided by
   * table's configuration.
   * @return SQL column definition
   */
  def sqlDefinition: String = table.configuration.dialect.columnDefinition(this)

  /**
   * Instantiates a field instance for the record.
   * Implementation should return column-specific field instance.
   * @return Field instance for this column
   */
  def apply(): Field[T] = new Field[T](this)

}

class StringColumn(table: Table, name: String) extends Column[String](table, name) {

  def sqlType = "varchar"

}

//class Person extends Record {
//  val name = Person.name()
//}
//
//object Person extends Table("public", "person") {
//  val name = new StringColumn(this, "name");
//}
//
//object Test extends Application {
//  val p = new Person()
//  println(p.getClass.getMethods.toList)
//}

