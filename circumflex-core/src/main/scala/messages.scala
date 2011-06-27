package ru.circumflex.core

import java.lang.String
import collection.{Iterator, Map}
import collection.JavaConversions._
import collection.mutable.{ArrayBuffer, HashMap}
import java.util.{ResourceBundle, Locale, Properties, Enumeration => JEnumeration, Date}
import java.text.MessageFormat
import java.io._
import org.apache.commons.io.FilenameUtils

/*!# Messages API

Messages API offers you a convenient way to internationalize your application.

Generally, all strings which should be presented to user are stored in
separate `.properties`-files as suggested by [Java Internationalization][java-i18n].

Circumflex Messages API goes beyond this simple approach and offers
delegating resolving, messages grouping, parameters interpolation and formatting.

  [java-i18n]: http://java.sun.com/javase/technologies/core/basic/intl

The usage is pretty simple: you use the `msg` method of package object `ru.circumflex.core`
which returns an implementation of `MessageResolver` used to retrieve messages. This instance
is also referred to as _global messages resolver_. By default, the `PropertyFileResolver`
is used. You can set `cx.messages` configuration parameter to use your own
`MessageResolver` implementation as global resolver.

The `resolve` method is responsible for resolving a message by `key`.

Circumflex Messages API features very robust ranged resolving. The message is searched
using the range of keys, from the most specific to the most general ones: if the message
is not resolved with given key, then the key is truncated from the left side to
the first dot (`.`) and the message is searched again. For example, if you are looking
for a message with the key `com.myapp.model.Account.name.empty` (possibly while performing
domain model validation), then following keys will be used to lookup an appropriate
message (until first success):

    com.myapp.model.Account.name.empty
    myapp.model.Account.name.empty
    model.Account.name.empty
    Account.name.empty
    name.empty
    empty

You can use the methods of Scala `Map` to retrieve messages from resolver.
Default implementation also reports missing messages into Circumflex debug log.

The locale is taken from `cx.locale` context variable (see `Context` for more details).
If no such variable found in the context, then the platform's default locale is used.

Messages can also be formatted. We support both classic `MessageFormat` style
(you know, with `{0}`s in text and varargs) and parameters interpolation (key-value pairs
are passed as arguments to `fmt` method, each `{key}` in message is replaced by
corresponding value).

You can use `ResourceBundleMessageResolver` to resolve messages from Java `ResourceBundle`s.

The default implementation (the `msg` method in package `ru.circumflex.core`)
uses uses property files with base name `Messages` to lookup messages. You can override
the default implementation by setting `cx.messages` configuration parameter. Unlike Java
`ResourceBundle` it effectively caches property files and allows hot editing (cache is
based on last modified dates).

You can set `cx.messages.root` to point to different directory (for example, to your webapp
root) and `cx.messages.name` to change the default base name of property files.

If you need to search messages in different sources, you can use
`DelegatingMessageResolver`: it tries to resolve a message using specified
`resolvers` list, the first successively resolved message is returned.
*/
trait MessageResolver extends Map[String, String] {
  protected var _lastModified = new Date()
  def lastModified = _lastModified

  def -(key: String): Map[String, String] = this
  def +[B1 >: String](kv: (String, B1)): Map[String, B1] = this

  protected def resolve(key: String): Option[String]

  protected def resolveRange(key: String): Option[String] = resolve(key) orElse {
    if (!key.contains(".")) None
    else resolveRange(key.substring(key.indexOf(".") + 1))
  }

  def get(key: String): Option[String] = resolveRange(key) orElse {
    CX_LOG.debug("Message with key '" + key + "' is missing.")
    None
  }

  def locale: Locale = ctx.get("cx.locale") match {
    case Some(l: Locale) => l
    case Some(l: String) => new Locale(l)
    case _ => Locale.getDefault
  }

  def fmt(key: String, params: (String, Any)*): String =
    params.foldLeft(getOrElse(key, "")) { (result, p) =>
      result.replaceAll("\\{" + p._1 + "\\}", p._2.toString)
    }
  def format(key: String, params: AnyRef*): String =
    MessageFormat.format(getOrElse(key, ""), params: _*)
}

class ResourceBundleMessageResolver(val bundleName: String) extends MessageResolver {
  protected def bundle = ResourceBundle.getBundle(
    bundleName, locale, Thread.currentThread.getContextClassLoader)
  def iterator: Iterator[(String, String)] = bundle.getKeys
      .map(k => (k -> bundle.getString(k)))
  protected def resolve(key: String): Option[String] =
    try { Some(bundle.getString(key)) } catch { case _ => None }
}

class DelegatingMessageResolver(initialResolvers: MessageResolver*) extends MessageResolver {
  protected var _resolvers: Seq[MessageResolver] = initialResolvers
  def resolvers = _resolvers
  def addResolver(r: MessageResolver): this.type = {
    _resolvers ++= List(r)
    _lastModified = new Date()
    return this
  }
  def iterator: Iterator[(String, String)] =
    resolvers.map(_.iterator).reduceLeft((a, b) => a ++ b)
  protected def resolve(key: String): Option[String] = {
    resolvers.foreach(r => r.get(key).map(msg => return Some(msg)))
    return None
  }
}

class PropertyFileResolver extends MessageResolver {
  val propsRoot = new File(
    FilenameUtils.separatorsToSystem(
      cx.getOrElse("cx.messages.root", "src/main/resources").toString))
  val resourceName = cx.getOrElse("cx.messages.name", "Messages").toString
  protected val _cache = new HashMap[String, (Properties, Long)]

  protected def getFile(suffix: String) =
    new File(propsRoot, resourceName + suffix + ".properties")

  protected def getProps(suffix: String): Option[Properties] = {
    val f = getFile(suffix)
    _cache.get(suffix) match {
      case Some((props: Properties, lm: Long)) =>
        if (!f.isFile) {    // previously cached file does not exist anymore
          _cache.remove(suffix)
          _lastModified = new Date()
          getProps(suffix)
        } else {
          if (f.lastModified > lm) {  // cached file has been modified
            _lastModified = new Date()
            loadProps(f) match {
              case Some(p: Properties) =>
                _cache(suffix) = (p, f.lastModified)
                Some(p)
              case None =>    // previously cached file does not exist anymore
                _cache.remove(suffix)
                getProps(suffix)
            }
          } else Some(props)      // not modified -- return cached
        }
      case _ => loadProps(f) map { p =>
        _cache(suffix) = (p, f.lastModified)
        p
      }
    }
  }

  protected def loadProps(file: File): Option[Properties] = {
    if (!file.isFile) None
    else {
      val is = new FileInputStream(file)
      val props = new Properties
      try {
        props.load(is)
      } finally {
        is.close
      }
      return Some(props)
    }
  }

  def fallbackSuffix(suffix: String): String = {
    val i = suffix.lastIndexOf("_")
    if (i == -1) return ""
    else return suffix.substring(0, i)
  }

  def localeSuffix = "_" + locale.toString

  def iterator: Iterator[(String, String)] = {
    var suffix = ""
    var result: Iterator[(String, String)] = iteratorInternal(suffix)
    localeSuffix.split("_").filter(_ != "").foreach { part =>
      suffix += "_" + part
      getProps(suffix) map { props => result ++= iteratorInternal(suffix) }
    }
    return result
  }

  protected def iteratorInternal(suffix: String): Iterator[(String, String)] =
    getProps(suffix).map { props =>
      props.keys.asInstanceOf[JEnumeration[String]].map(k => k -> props.getProperty(k))
    }.getOrElse(Iterator.empty)

  protected def resolve(key: String): Option[String] = resolveInternal(key, localeSuffix)

  protected def resolveInternal(key: String, suffix: String): Option[String] =
    getProps(suffix).flatMap(props => any2option(props.getProperty(key))).orElse {
      if (suffix == "") None
      else resolveInternal(key, fallbackSuffix(suffix))
    }

}

case class Msg(key: String, params: (String, Any)*) {
  def param(key: String): Option[Any] = params.find(_._1 == key).map(_._2)
  def hasParam(key: String): Boolean = !params.find(_._1 == key).isEmpty
  override def toString: String = msg.fmt(key, params: _*)
}
