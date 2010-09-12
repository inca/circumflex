package ru.circumflex.orm

import ORM._

/*!# Field

The `Field` class holds atomic values of records. It wraps actual value
and provides methods for constructing column definitions for enclosing
tables. It also contains the `REFERENCES` method which is used to create
associations.
 */
class Field[T](record: Record[_],
               name: String,
               sqlType: String) extends ValueHolder[T](record, name, sqlType) {

}