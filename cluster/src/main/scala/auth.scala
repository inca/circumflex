package pro.savant.circumflex
package cluster

import core._, security._, xml._
import java.io.File

class User
    extends Principal
    with StructHolder {

  def elemName = "user"

  val _name = attr("name")
  def name = _name.getOrElse("")

  def setName(n: String) {
    _name := n
  }

  val _passwdSha256 = attr("passwdSha256")
  def passwdSha256 = _passwdSha256.getOrElse("")

  def setPassword(p: String) {
    _passwdSha256 := sha256(p)
  }

  def uniqueId = name

  def secret = passwdSha256

}

object Anonymous extends User {
  override def name = "anonymous"
  override def passwdSha256 = ""
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
