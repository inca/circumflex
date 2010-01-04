/*
 * Copyright (C) 2009-2010 Boris Okunskiy (http://incarnate.ru)
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

package ru.circumflex.orm

class A extends Record[A] {
  val id = field(A.id)
  val b = oneToMany(B.a)
}

object A extends Table[A] with LongIdPK[A]

class B extends Record[B] {
  val id = field(B.id)
  val a = manyToOne(B.a)
  val d = manyToOne(B.d)
  val c = oneToMany(C.b)
}

object B extends Table[B] with LongIdPK[B] {
  val a = longColumn("a")
          .notNull
          .references(A)
  val d = longColumn("d")
          .references(D)
          .onDeleteSetNull
}

class C extends Record[C] {
  val b = manyToOne(C.b)
}

object C extends Table[C] with LongIdPK[C] {
  val b = longColumn("b")
          .notNull
          .references(B)
}

class D extends Record[D] {
  val id = field(D.id)
  val b = oneToMany(B.d)
}

object D extends Table[D] with LongIdPK[D]

object Init {

  def schema = new DDLExport(A, B, C, D).dropCreate

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
    c1.save
    val c3 = new C
    c3.b := b3
    c3.save
    val c4 = new C
    c4.b := b3
    c4.save
    val c5 = new C
    c5.b := b4
    c5.save
    ORM.connectionProvider.getConnection.commit
  }

  val aTree = A as "a" join (B as "b" join (C as "c") join (D as "d"))

  val bTree = B as "b" join (A as "a") join (C as "c") join (D as "d")

}