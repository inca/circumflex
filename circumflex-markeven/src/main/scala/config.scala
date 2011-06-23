package ru.circumflex.markeven

import collection.mutable.HashMap

/*!# Markeven Configuration

`MarkevenConfiguration` allows application to customize various processing features:

  * macro processors;
  * link resolvers;
  * custom URL processors;
  * pre- and post- processors.
*/

trait MarkevenConfiguration {
  def resolveLink(id: String): Option[LinkDefinition] = None

  /*! Macros allow integrating your own stuff. For example, following code fragment:

          conf.addMacro("my_macro")(s => s.toString.toUpperCase)

      will make text `[[my_macro:this text is uppercased]]` appear like `THIS TEXT IS UPPERCASED`.
   */

  val macros = new HashMap[String, StringEx => CharSequence]()
  def addMacro(name: String)(function: StringEx => CharSequence): this.type = {
    macros += (name -> function)
    return this
  }

  /*! Default indentation for complex blocks is two spaces. You can override this here. */
  def indent: String = "  "


}