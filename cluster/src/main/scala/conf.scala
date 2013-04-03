package pro.savant.circumflex
package cluster

import core._, xml._
import java.io.File

class ClusterConfiguration
    extends StructHolder
    with XmlFile {

  def elemName = "cluster"

  def descriptorFile = new File(root, "cluster.xml")

  val users = new ListHolder[User] {

    def elemName = "users"

    def read = {
      case "user" => new User
    }

  }

}