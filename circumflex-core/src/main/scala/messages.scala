package ru.circumflex.core

import java.lang.String
import collection.{Iterator, Map}
import collection.JavaConversions._
import java.util.{ResourceBundle, Locale}
import java.text.MessageFormat

/*!# Messages API

Messages API offers you a convenient way to internationalize your application.

Generally, all strings that should be presented to user are stored in
separate `.properties`-files as suggested by [Java Internationalization][java-i18n].

Circumflex Messages API goes beyound this simple approach and offers
delegating resolving, messages grouping, parameters interpolation and formatting.

  [java-i18n]: http://java.sun.com/javase/technologies/core/basic/intl

The usage is pretty simple: you use the `msg` method of package object `ru.circumflex.core`
which returns an implementation of `MessageResolver` used to retrieve messages. This instance
is also refered to as *global messages resolver*. By default, the `DefaultMessageResolver`
singleton is used. You can set `cx.messages` configuration parameter to use your own
`MessageResolver` implementation as global resolver.
*/

/**
 * Provides an interface for resolving messages.
 *
 * For more information refer to
 * <a href="http://circumflex.ru/api/2.0/circumflex-core/messages.scala">messages.scala</a>
 */
trait MessageResolver extends Map[String, String] {
  def -(key: String): Map[String, String] = this
  def +[B1 >: String](kv: (String, B1)): Map[String, B1] = this

  /*! The `resolve` method is responsible for resolving a message by `key`. */
  protected def resolve(key: String): Option[String]

  /*! Circumflex Messages API features very robust ranged resolving. The message is searched
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
  */
  protected def resolveRange(key: String): Option[String] = resolve(key) orElse {
    if (!key.contains(".")) None
    else resolveRange(key.substring(key.indexOf(".") + 1))
  }

  /*! You can use the methods of Scala `Map` to retrieve messages from resolver.
  Default implementation also reports missing messages into Circumflex debug log.
  */
  def get(key: String): Option[String] = resolveRange(key) orElse {
    CX_LOG.debug("Message with key '" + key + "' is missing.")
    None
  }

  /*! The locale is taken from `cx.locale` context variable (see `Context` for more details).
  If no such variable found in the context, then the platform's default locale is used.
  */
  def locale: Locale = ctx.get("cx.locale") match {
    case Some(l: Locale) => l
    case Some(l: String) => new Locale(l)
    case _ => Locale.getDefault
  }

  /*! Messages can also be formatted. We support both classic `MessageFormat` style
  (you know, with `{0}`s in and varargs) and parameters interpolation (key-value pairs
  are passed as arguments to `fmt` method, each `{key}` in message is replaced by
  corresponding value).
  */
  def fmt(key: String, params: Pair[String, Any]*): String =
    params.foldLeft(getOrElse(key, "")) {
      (result, p) => result.replaceAll("\\{" + p._1 + "\\}", p._2.toString)
    }
  def format(key: String, params: AnyRef*): String =
    MessageFormat.format(getOrElse(key, ""), params: _*)
}

/*! You can use `ResourceBundleMessageResolver` to resolve messages from Java `ResourceBundle`s. */


/**
 * Resolves messages from `ResourceBundle` with specified `bundleName`.
 *
 * For more information refer to
 * <a href="http://circumflex.ru/api/2.0/circumflex-core/messages.scala">messages.scala</a>
 */
class ResourceBundleMessageResolver(val bundleName: String) extends MessageResolver {
  protected def bundle = ResourceBundle.getBundle(bundleName, locale)
  def iterator: Iterator[(String, String)] = bundle.getKeys
      .map(k => (k -> bundle.getString(k)))
  protected def resolve(key: String): Option[String] =
    try { Some(bundle.getString(key)) } catch { case _ => None }
}

/*! The default implementation (the `msg` method in package `ru.circumflex.core`)
uses `ResourceBundle` with base name `Messages` to lookup messages. You can override
the default implementation by setting `cx.messages` configuration parameter.
*/

/**
 * Resolves messages from `ResourceBundle` with base name `Messages`.
 *
 * For more information refer to
 * <a href="http://circumflex.ru/api/2.0/circumflex-core/messages.scala">messages.scala</a>
 */
object DefaultMessageResolver extends ResourceBundleMessageResolver("Messages")

/*! If you need to search messages in different sources, you can use
`DelegatingMessageResolver`: it tries to resolve a message using specified
`resolvers` list, the first successively resolved message is returned.
*/

/**
 * Resolves messages by delegating calls to specified `initialResolvers`.
 *
 * For more information refer to
 * <a href="http://circumflex.ru/api/2.0/circumflex-core/messages.scala">messages.scala</a>
 */
class DelegatingMessageResolver(initialResolvers: MessageResolver*) {
  protected var _resolvers: Seq[MessageResolver] = initialResolvers
  def resolvers = _resolvers
  def addResolver(r: MessageResolver): this.type = {
    _resolvers ++= List(r)
    return this
  }
  def iterator: Iterator[(String, String)] =
    resolvers.map(_.iterator).reduceLeft((a, b) => a ++ b)
  protected def resolve(key: String): Option[String] = {
    resolvers.foreach(r => r.get(key).map(msg => return Some(msg)))
    return None
  }
}


