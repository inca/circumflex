package ru.circumflex
package diff

trait Equalizer {
  def equals(original: Any, revised: Any): Boolean
}

class MyersDiff extends DiffAlgorithm {
  private object DEFAULT_EQUALIZER extends Equalizer {
    def equals(original: Any, revised: Any) = original.equals(revised)
  }

  protected var _equalizer: Equalizer = DEFAULT_EQUALIZER

  def this(equalizer: Equalizer) {
    this()
    _equalizer = equalizer
  }

  def diff(original: Seq[_], revised: Seq[_]): Patch = {
    try {
      val path = buildPath(original, revised)
      return buildRevision(path, original, revised)
    } catch {
      case e: Exception =>
        DIFF_LOG.error(e.printStackTrace())
    }
    new Patch
  }

  def buildPath(original: Seq[_], revised: Seq[_]): PathNode = {
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

        while (i < N && j < M && equals(original(i), revised(j))) {
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

  @inline private def equals(orig: Any, rev: Any): Boolean = _equalizer.equals(orig, rev)

  def buildRevision(path: PathNode, original: Seq[_], revised: Seq[_]) = {
    val patch = new Patch
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
      val orig = new Chunk(ianchor, copyOfRange(original, ianchor, i))
      val rev = new Chunk(janchor, copyOfRange(revised, janchor, j))
      val delta: Delta =
        if (orig.size == 0 && rev.size != 0)
          new InsertDelta(orig, rev)
        else if (orig.size >  0 && rev.size == 0)
          new DeleteDelta(orig, rev)
        else new ChangeDelta(orig, rev)

      patch.addDelta(delta)
      if (path.isSnake) pathOpt = path.prev
    }
    patch
  }

  def copyOfRange(original: Seq[Any],
                     from: Int, to: Int): Seq[Any] = {
    val newLength = to - from;
    if (newLength < 0)
      throw new IllegalArgumentException(from + " > " + to)
    var copy = new Array[Any](math.min(original.length - from, newLength))
    System.arraycopy(original.toArray, from, copy, 0, copy.length)
    copy
  }
}