package ru.circumflex.orm

/*!# Association

The `Association` class lets you create associations between records which are represented
by foreign key constraints in database. This kind of relationship is often refered to as
one-to-one or many-to-one (the former is implemented by adding a `UNIQUE` constraint).
*/
class Association[C <: Record, P <: Record, K](val field: Field[K],
                                               val parentClass: Class[P])
    extends Field[P](field.record, field.name, field.sqlType) {

  

}