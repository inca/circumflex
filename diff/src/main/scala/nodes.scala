package pro.savant.circumflex
package diff

abstract class PathNode(val i: Int,
               val j: Int,
               val prev: Option[PathNode]) {

  def isSnake: Boolean
  def isBootstrap = i < 0 || j < 0

  def previousSnake: Option[PathNode] = {
    if (isBootstrap)
      return None
    if (!isSnake && !prev.isEmpty)
      return prev.get.previousSnake
    Some(this)
  }

  override def toString = {
    val sb = new StringBuffer("[")
    var node: Option[PathNode] = Some(this)
    while (!node.isEmpty) {
      sb.append("(")
          .append(i.toString)
          .append(",")
          .append(j.toString)
          .append(")")
      node = node.get.prev
    }
    sb.append("]")
    sb.toString
  }
}

class Snake(i: Int, j: Int, prev: Option[PathNode])
    extends PathNode(i, j, prev) {
  def isSnake = true
}

class DiffNode(i: Int, j: Int, prev: Option[PathNode])
    extends PathNode(i, j, if (prev.isEmpty) None else prev.get.previousSnake) {
  def isSnake = false
}