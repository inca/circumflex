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
  def hasNext: Boolean = (index + 1) < _buffer.length
  def next: Diff = {
    _index += 1
    return _buffer(index)
  }
  def previous: Diff = {
    _index -= 1
    return _buffer(index)
  }
  def remove(): Diff = _buffer.remove(index)
  def add(d: Diff) = {
    _buffer.insert(index, d)
    _index += 1
  }
  def all: Seq[Diff] = _buffer.toSeq
}

class EditCounter {
  protected var _deleteCount = 0
  protected var _deleteText = ""
  protected var _insertCount = 0
  protected var _insertText = ""

  def deleteCount = _deleteCount
  def deleteText = _deleteText
  def insertCount = _insertCount
  def insertText = _insertText

  def delete(text: String): Unit = {
    _deleteCount += 1
    _deleteText += text
  }

  def insert(text: String): Unit = {
    _insertCount += 1
    _insertText += text
  }

  def reset(): Unit = {
    _deleteCount = 0
    _deleteText = ""
    _insertCount = 0
    _insertText = ""
  }

  def hasBoth(): Boolean = (_deleteCount > 0 && _insertCount > 0)

}