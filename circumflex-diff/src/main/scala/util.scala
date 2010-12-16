package ru.circumflex.diff

import collection.mutable.{HashMap, ListBuffer}

case class HalfMatch(prefix1: String,
                     suffix1: String,
                     prefix2: String,
                     suffix2: String,
                     common: String) {
  def inverse = HalfMatch(prefix2, suffix2, prefix1, suffix1, common)
}

case class EncodedLines(chars1: String, chars2: String, lines: Seq[String])

class DiffIterator(diffs: Seq[Diff] = Nil) {
  protected val _buffer = ListBuffer[Diff](diffs: _*)
  protected var _index = -1
  def index = _index
  def hasMore(qty: Int): Boolean = (index + qty) < _buffer.length
  def hasNext: Boolean = hasMore(1)
  def next: Diff = {
    _index += 1
    return _buffer(index)
  }
  def hasPrevious: Boolean = index > 0
  def previous: Diff = {
    _index -= 1
    return _buffer(index)
  }
  def hasCurrent: Boolean = (index >= 0 && index < _buffer.length)
  def current: Diff = _buffer(index)
  def remove(): Diff = {
    val d = _buffer.remove(index)
    _index -= 1
    return d
  }
  def add(d: Diff) = {
    _index += 1
    _buffer.insert(index, d)
  }
  def replace(diff: Diff, i: Int = index): Unit = {
    _buffer.remove(i)
    _buffer.insert(i, diff)
  }
  def all: Seq[Diff] = _buffer.toSeq
}

class EditCounter {
  var deleteCount = 0
  var deleteText = ""
  var insertCount = 0
  var insertText = ""

  def delete(text: String): Unit = {
    deleteCount += 1
    deleteText += text
  }

  def insert(text: String): Unit = {
    insertCount += 1
    insertText += text
  }

  def reset(): Unit = {
    deleteCount = 0
    deleteText = ""
    insertCount = 0
    insertText = ""
  }

  def hasBoth(): Boolean = (deleteCount > 0 && insertCount > 0)
  def hasEither(): Boolean = (deleteCount > 0 || insertCount > 0)

}