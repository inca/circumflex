package ru.circumflex.orm.i18n

import ru.circumflex.orm._

class Foo extends Record[Foo] {
  val key = field(Foo.key)
  val name = field(Foo.name)
}

object Foo extends LocalizableView[Foo]
        with LongIdPK[Foo] {
  val key = stringColumn("key")
          .notNull
          .unique
  val name = stringColumn("name")
          .notNull
  localize(name)
}

class Bar extends Record[Bar] {
  val name = field(Bar.name)
  val foo = manyToOne(Bar.foo)
}

object Bar extends LocalizableView[Bar]
        with LongIdPK[Bar] {
  val name = stringColumn("name")
          .notNull
  val foo = longColumn("foo_id")
          .notNull
          .references(Foo)
          .onDeleteCascade
  localize(name)
}