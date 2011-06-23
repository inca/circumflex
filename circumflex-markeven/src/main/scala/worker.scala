package ru.circumflex.markeven

import collection.mutable.{HashMap, ListBuffer}

/*!# Markeven Worker

`MarkevenWorker` is a class which performs actual processing. It should be created for each processing task
and should never be shared by different threads or tasks.
 */
class MarkevenWorker(conf: MarkevenConfiguration, input: CharSequence) {
  protected val buffer = new StringEx(input)
  protected val blocks = new ListBuffer[Block]
  protected val protector = new Protector
  protected val linkDefs = new HashMap[String, LinkDefinition]

  def resolveLink(id: String): Option[LinkDefinition] = linkDefs.get(id).orElse(conf.resolveLink(id))

  // Indentation stuff

  protected var level = 0

  def increaseIndent: Unit = level += 1
  def decreaseIndent: Unit = if (level > 0) level -= 1

  def currentIndent: String =
    if (level <= 0) return ""
    else conf.indent * level

}