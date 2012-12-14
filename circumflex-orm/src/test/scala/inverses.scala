package pro.savant.circumflex
package orm

// Testing inverse associations

class Person extends Record[String, Person] {
  def PRIMARY_KEY = name

  val name = "name".TEXT.NOT_NULL

  def comments = inverseMany(Comment.owner)
  def devices = inverseMany(Device.owner)
  def documents = inverseMany(Document.owner)

  def relation = Person

  override def toString = name.getOrElse("<unknown>")
}

object Person extends Person with Table[String, Person]

trait OwnedRecord[R <: OwnedRecord[R]]
    extends Record[Long, R]
    with IdentityGenerator[Long, R] { this: R =>

  def PRIMARY_KEY = id

  val id = "id".BIGINT.NOT_NULL.AUTO_INCREMENT
  val owner = "owner_id".TEXT.NOT_NULL.REFERENCES(Person)

}

class Comment extends OwnedRecord[Comment] {
  def relation = Comment
}

object Comment extends Comment with Table[Long, Comment]

class Device extends OwnedRecord[Device] {
  def relation = Device
}

object Device extends Device with Table[Long, Device]

class Document extends OwnedRecord[Document] {
  def relation = Document
}

object Document extends Document with Table[Long, Document]

