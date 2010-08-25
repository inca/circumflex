package ru.circumflex.core

import java.lang.String
import collection.{Iterator, Map}
import collection.mutable.HashMap
import java.util.{ResourceBundle, Locale}
import java.text.MessageFormat

/*!# Messages API

Messages API offers you a convenient way to internationalize your application.

Generally, all strings that should be presented to user are stored in
separate `.properties`-files as suggested by [Java Internationalization][java-i18n].

Circumflex Messages API goes beyound this simple approach and offers
delegating resolving, messages grouping, parameters interpolation and formatting.

  [java-i18n]: http://java.sun.com/javase/technologies/core/basic/intl
*/
trait MessageResolver extends Map[String, String] {
  def -(key: String): Map[String, String] = this
  def +[B1 >: String](kv: (String, B1)): Map[String, B1] = this

  /*! `MessageResolver` is responsible for resolving a message by `key`. */
  protected def resolve(key: String): Option[String]

  /*! Circumflex Messages API features very robust ranged resolving. The message is searched
  using the range of keys, from the most specific to the most general ones: if the message
  is not resolved with given key, then the key is truncated from the left side to
  the first dot (`.`) and the message is searched again. For example, if you are looking
  for a message with key `com.myapp.model.Account.name.empty` (possibly while performing
  domain model validation), then following keys will be used to lookup an appropriate
  message:

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

  def get(key: String): Option[String] = resolveRange(key) orElse {
    CX_LOG.debug("Message with key '" + key + "' is missing.")
    None
  }

  /*! The locale is taken from `cx.locale` context variable (see `Context` for more details).
  If no such variable found in context, platform's default locale is used.
  */
  def locale: Locale = Circumflex.get("cx.locale") match {
    case Some(l: Locale) => l
    case Some(l: String) => new Locale(l)
    case _ => Locale.getDefault
  }

  /*! Messages can also be formatted. We support both classic `MessageFormat` style
  and parameters interpolation (key-value pairs are passed as arguments to `fmt`
  method, each `{key}` in message is replaced by corresponding value).
  */
  def fmt(key: String, params: Pair[String, Any]*): String =
    params.foldLeft(getOrElse(key, "")) {
      (result, p) => result.replaceAll("\\{" + p._1 + "\\}", p._2.toString)
    }
  def format(key: String, params: Any*): String =
    MessageFormat.format(getOrElse(key, ""), params.toArray)
}

/*! You can use `ResourceBundleMessageResolver` to resolve messages from Java `ResourceBundle`s. */
class ResourceBundleMessageResolver(val bundleName: String) extends MessageResolver {
  protected val bundle = ResourceBundle.getBundle(bundleName, locale)
  def iterator: Iterator[(String, String)] = bundle.getKeys
      .map(k => (k -> bundle.getString(k)))
  protected def resolve(key: String): Option[String] =
    try { Some(bundle.getString(key)) } catch { case _ => None }
}

/*! The default implementation (`msg` method in package `ru.circumflex.core`)
uses `ResourceBundle` with base name `Messages` to lookup messages. You can override
the default implementation by setting `cx.messages` configuration parameter.
*/
object DefaultMessageResolver extends ResourceBundleMessageResolver("Messages")

/*! If you need to search messages in different sources, you can use
`DelegatingMessageResolver`: it tries to resolve a message using specified
`resolvers` list, the first successively resolved message is returned.
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


