package ru.circumflex.orm.util

import java.text.MessageFormat

/**
 * Utility class to combine StringBuilder with MessageFormat and
 * some other useful String utils
 */
class StringEx {

  def this(value: String) = {
    this()
    append(value)
  }

  def this(value: String, args: Object*) = {
    this()
    append(value, args)
  }

  private val builder = new StringBuilder

  def append(value: String, args: Object*): StringEx = {
    val argsList: List[Object] = args.toList
    if (argsList.size > 0)
      builder.append(MessageFormat.format(value.replaceAll("'","''"), argsList: _*))
    else builder.append(value);
    return this
  }

  def backspace(times: Int): StringEx = {
    builder.delete(builder.length - times, builder.length)
    return this
  }

  override def toString = builder.toString.trim

}
