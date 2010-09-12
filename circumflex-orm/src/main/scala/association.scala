package ru.circumflex.orm

/*!# Association

The `Association` class lets you create associations between records which are represented
by foreign key constraints in database. This kind of relationship is often refered to as
one-to-one or many-to-one (the former is implemented by adding a `UNIQUE` constraint).

We use some terminology when speaking about associations. The `C` type parameter is used to
point to the table which holds this association (we refer to this table as the "child relation"),
the `P` type parameter, accordingly, is used to point to the "foreign" table (we refer to this table
as the "parent relation"). The `K` type parameter corresponds to the type of primary key of parent
relation (and, therefore, to the type of corresponding field of child relation to which a foreign key
constraint is applied).
*/
class Association[K, C <: Record[_], P <: Record[K]](val field: Field[K],
                                                     val parentClass: Class[P])
    extends ValueHolder[P](field.record, field.name, field.sqlType) {

  /*!## Column Definition Methods

  The methods from `ValueHolder` responsible for column definition are delegated
  to an internal `field`.
  */
  override def NOT_NULL(): this.type = {
    field.NOT_NULL
    return this
  }
  override def notNull_?(): Boolean =
    field.notNull_?
  override def UNIQUE(): this.type = {
    field.UNIQUE
    return this
  }
  override def unique_?(): Boolean =
    field.unique_?
  override def DEFAULT(expr: String): this.type = {
    field.DEFAULT(expr)
    return this
  }
  override def defaultExpression: Option[String] =
    field.defaultExpression

}