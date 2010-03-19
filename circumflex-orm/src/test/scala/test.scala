package ru.circumflex.orm

import ORM._
import xml.XML

class Country extends Record[Country] {
  val id = field(Country.id)
  val key = field(Country.key)
  val name = field(Country.name)
  val cities = oneToMany(City.country)
}

object Country extends Table[Country]
        with LongIdPK[Country] {
  val key = stringColumn("key")
          .notNull
          .unique
  val name = stringColumn("name")
          .notNull
}

class City extends Record[City] {
  val id = field(City.id)
  val key = field(City.key)
  val name = field(City.name)
  val country = manyToOne(City.country)
}

object City extends Table[City]
        with LongIdPK[City] {
  val key = stringColumn("key")
          .notNull
          .unique
  val name = stringColumn("name")
          .notNull
  val country = longColumn("country_id")
          .notNull
          .references(Country)
          .onDeleteCascade
}

class A extends Record[A] {
  val id = field(A.id)
  val b = oneToMany(B.a)
  override def toString = "A" + id.getOrElse(0)
}

object A extends Table[A] with LongIdPK[A]

class B extends Record[B] {
  val id = field(B.id)
  val a = manyToOne(B.a)
  val d = manyToOne(B.d)
  val c = oneToMany(C.b)
  override def toString = "B" + id.getOrElse(0)
}

object B extends Table[B] with LongIdPK[B] {
  val a = longColumn("a")
          .notNull
          .references(A)
  val d = longColumn("d")
          .references(D)
          .onDeleteSetNull
  prefetch(a, d, C.b)
}

class C extends Record[C] {
  val id = field(C.id)
  val b = manyToOne(C.b)
  override def toString = "C" + id.getOrElse(0)
}

object C extends Table[C] with LongIdPK[C] {
  val b = longColumn("b")
          .notNull
          .references(B)
}

class D extends Record[D] {
  val id = field(D.id)
  val b = oneToMany(B.d)
  override def toString = "D" + id.getOrElse(0)
}

object D extends Table[D] with LongIdPK[D]

object Init {

  def schema = new DDLExport(A, B, C, D, Country, City).dropCreate

  def data = {
    val a1 = new A
    a1.save
    val a2 = new A
    a2.save
    val d1 = new D
    d1.save
    val b1 = new B
    b1.a := a1
    b1.d := d1
    b1.save
    val b2 = new B
    b2.a := a1
    b2.d := d1
    b2.save
    val b3 = new B
    b3.a := a2
    b3.d := d1
    b3.save
    val b4 = new B
    b4.a := a2
    b4.save
    val c1 = new C
    c1.b := b1
    c1.save
    val c2 = new C
    c2.b := b1
    c2.save
    val c3 = new C
    c3.b := b3
    c3.save
    val c4 = new C
    c4.b := b3
    c4.save
    val c5 = new C
    c5.b := b4
    c5.save
    tx.commit
  }

  def deps = Deployment.readAll(XML.load(
    this.getClass.getResourceAsStream("/test.cxd.xml")))

}
