package ru.circumflex.orm

/*!# Schema objects

Following classes represent various database schema objects:

  * `Schema` corresponds to database schema (or catalog), if such objects
  are supported by database vendor;
  * `Constraint` corresponds to one of database constraint types:

    * `Unique`,
    * `ForeignKey`,
    * `CheckConstraint`;

  * `Index` corresponds to database index.

Circumflex ORM also uses some helpers to make DSL-style data definition.
*/

class Schema(val name: String) extends SchemaObject {
  def objectName = "SCHEMA " + name
  def sqlCreate = "todo"
  def sqlDrop = "todo"
}
