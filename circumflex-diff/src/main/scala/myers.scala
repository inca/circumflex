package ru.circumflex
package diff

import collection.mutable.ListBuffer

class MyersDiff[T] {

  def diff(original: Seq[T], revised: Seq[T]): Patch[T] = try {
    val path = buildPath(original, revised)
    buildRevision(path, original, revised)
  } catch {
    case e: Exception =>
      DIFF_LOG.error("error", e)
      throw e
  }

  def buildPath(original: Seq[T], revised: Seq[T]): PathNode = {
    val N = original.length
    val M = revised.length
    val MAX = M + N + 1
    val size = 1 + 2 * MAX
    val middle = size / 2
    val diagonal = new Array[PathNode](size)

    diagonal(middle + 1) = new Snake(0, -1, None)
    0.until(MAX).map { d =>
      (-d).to(d, 2).map { k =>
        val kmiddle = middle + k
        val kplus = kmiddle + 1
        val kminus = kmiddle -1

        var prev: Option[PathNode] = None
        var i: Int =
          if ((k == -d) || (k != d && diagonal(kminus).i < diagonal(kplus).i)) {
            prev = if (diagonal(kplus) == null) None else Some(diagonal(kplus))
            diagonal(kplus).i
          } else {
            prev = if (diagonal(kminus) == null) None else Some(diagonal(kminus))
            diagonal(kminus).i + 1
          }
        diagonal(kminus) = null
        var j = i - k
        var node: PathNode = new DiffNode(i, j, prev)

        while (i < N && j < M && (original(i) == revised(j))) {
          i += 1
          j += 1
        }

        if (i > node.i)
          node = new Snake(i, j, Some(node))

        diagonal(kmiddle) = node

        if (i >= N && j >= M)
          return diagonal(kmiddle)
      }
      diagonal(middle + d - 1) = null
    }
    // According to Myers, this cannot happen
    throw new DifferentiationFailedException("Could not find a diff path")
  }

  def buildRevision(path: PathNode, original: Seq[T], revised: Seq[T]) = {
    val patch = new Patch[T]
    var pathOpt: Option[PathNode] = Some(path)
    if (path.isSnake) {
      pathOpt = path.prev
    }
    while (!pathOpt.isEmpty
        && !pathOpt.get.prev.isEmpty
        && pathOpt.get.prev.get.j >= 0) {
      var path = pathOpt.get
      if (path.isSnake)
        throw new IllegalStateException("bad diffpath: found snake when looking for diff")
      var i = path.i
      var j = path.j
      path = path.prev.get
      var ianchor = path.i
      var janchor = path.j
      val orig = new Chunk[T](ianchor, copyOfRange(original, ianchor, i))
      val rev = new Chunk[T](janchor, copyOfRange(revised, janchor, j))
      val delta: Delta[T] =
        if (orig.size == 0 && rev.size != 0)
          new InsertDelta[T](orig, rev)
        else if (orig.size >  0 && rev.size == 0)
          new DeleteDelta[T](orig, rev)
        else new ChangeDelta[T](orig, rev)

      patch.addDelta(delta)
      if (path.isSnake) pathOpt = {
        // now it is equal time
        val orig = new Chunk[T](path.prev.get.i, copyOfRange(original, path.prev.get.i, path.i))
        val rev = new Chunk[T](path.prev.get.j, copyOfRange(revised, path.prev.get.j, path.j))
        val delta = new EqualDelta[T](orig, rev)
        patch.addDelta(delta)
        path.prev
      }
    }
    patch
  }

  def copyOfRange(original: Seq[T],
                  from: Int, to: Int): Seq[T] = {
    val buffer = new ListBuffer[T]
    val newLength = to - from;
    if (newLength < 0)
      throw new IllegalArgumentException(from + " > " + to)
    val len = math.min(original.length - from, newLength)
    buffer ++= original.drop(from).take(len)
    buffer.toSeq
  }
}