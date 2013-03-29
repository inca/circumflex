package pro.savant.circumflex
package nm

import core._, security._, xml._
import java.io.File

class User
    extends Principal
    with StructHolder {

  def elemName = "user"

  val _name = attr("name")
  def name = _name.getOrElse("")

  val _password = attr("password")
  def password = _password.getOrElse("")

  def uniqueId = name

  def secret = sha256(password)

}

object Anonymous extends User {
  override def name = "anonymous"
  override def password = ""
}

object auth
    extends Auth[User]
    with ListHolder[User]
    with XmlFile {

  def elemName = "users"

  def read = {
    case "user" => new User
  }

  def descriptorFile = new File(root, "users.xml")

  def lookup(principalId: String) = children.find(_.uniqueId == principalId)

  def defaultReturnLocation = "/"

  def anonymous = Anonymous

  def secureDomain = domain

  def loginUrl = secureOrigin + "/login"

  override def secureScheme = "http"

  load()

}



