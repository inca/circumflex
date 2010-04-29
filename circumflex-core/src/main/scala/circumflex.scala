package ru.circumflex.core

import java.io.File
import java.util.{Locale, ResourceBundle}
import org.apache.commons.io.FilenameUtils._
import javax.servlet.http.HttpServletRequest


/**
 * Circumflex configuration
 */
object Circumflex extends HashModel {

  private val _params = MutableMap[String, Any]()

  def apply(key: String): Option[Any] = _params.get(key)

  def update(key: String, value: Any) { _params(key) = value }

  // ### Defalts

  // should filter process request?
  this("cx.process_?") = { (r: HttpServletRequest) => !r.getRequestURI.toLowerCase.matches("/public/.*") }
  this("cx.root") = "src/main/webapp" // webapp root
  this("cx.public") = "public" // static files directory (relative to webapp root)
  this("cx.messages") = "Messages" // resource bundle for messages

  // ### Read configuration from `cx.properties` file

  try {
    val bundle = ResourceBundle.getBundle("cx", Locale.getDefault, classLoader)
    val keys = bundle.getKeys
    while (keys.hasMoreElements) {
      val k = keys.nextElement
      this(k) = bundle.getString(k)
    }
  } catch {
    case _ => cxLog.warn("Could not read configuration parameters from cx.properties.")
  }

  // ### Parameters

  val webappRoot: File = this("cx.root") match {
    case Some(s: String) => new File(separatorsToSystem(s))
    case _ => throw new CircumflexException("'cx.root' not configured.")
  }

  val publicRoot: File = this("cx.public") match {
    case Some(s: String) => new File(webappRoot, separatorsToSystem(s))
    case _ => throw new CircumflexException("'cx.public' not configured.")
  }

  def messages(locale: Locale): Option[Messages] = this("cx.messages") match {
    case Some(s: String) => new Messages(s, locale)
    case _ => None
  }

  // ### Loaders

  val classLoader: ClassLoader = this("cx.classLoader") match {
    case Some(cld: ClassLoader) => cld
    case _ => Thread.currentThread.getContextClassLoader
  }

  def loadClass[C](name: String): Class[C] =
    Class.forName(name, true, classLoader).asInstanceOf[Class[C]]

  def newObject[C](name: String, default: =>C): C = this(name) match {
    case Some(h: C) => h
    case Some(c: Class[C]) => c.newInstance
    case Some(s: String) => loadClass[C](s).newInstance
    case _ => default
  }

}

class CircumflexException(msg: String, cause: Throwable = null)
    extends Exception(msg, cause) {
  def this(cause: Throwable) = this(null, cause)
}