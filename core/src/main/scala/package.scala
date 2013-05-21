package pro.savant.circumflex

import java.security.MessageDigest
import java.util.{UUID, Random}
import java.util.regex.Pattern

/*!# The `core` package

Package object `core` contains different shortcuts, utilities and implicits.
You should import this package if you intend to use Circumflex API:

``` {.scala}
import pro.savant.circumflex.core._
````
*/
package object core {

  val CX_LOG = new Logger("pro.savant.circumflex.core")

  /*! Common Circumflex helpers imported into the namespace are:

  * `cx` for accessing Circumflex configuration singleton;
  * `ctx` for accessing the context -- special key-value storage attached
    to currently executing thread;
  * `msg` for accessing localized messages defined in `Messages.properties` (by default);
  */
  val cx = Circumflex
  def ctx = Context.get()
  lazy val msg = cx.instantiate[MessageResolver](
    "cx.messages", new PropertyFileResolver)

  /*! Circumflex Core package also includes helpers for various common tasks like
  random generation of UUIDs and alphanumeric strings, converting between camelCase and
  underscore_delimited identifiers, measuring execution time, computing popular digests
  (SHA256, MD5), etc.
  */

  protected val _rnd = new Random
  protected val _CHARS = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

  def randomString(length: Int) = (0 until length)
      .map(i => _CHARS.charAt(_rnd.nextInt(_CHARS.length)))
      .mkString
  def randomUUID = UUID.randomUUID.toString

  // Utils

  def camelCaseToUnderscore(arg: String) = arg.replaceAll("(?<!^)([A-Z])","_$1").toLowerCase

  def time[T](block: => T): (Long, T) = {
    val startTime = System.currentTimeMillis
    val result = block
    (System.currentTimeMillis - startTime, result)
  }

  def any2option[T](obj: T): Option[T] = if (obj == null) None else Some(obj)

  def digest(algorithm: String, text: String) = {
    val md = MessageDigest.getInstance(algorithm)
    md.update(text.getBytes)
    md.digest()
        .map(b => Integer.toHexString(0xFF & b))
        .map(b => if (b.length == 1) "0" + b else b)
        .mkString
  }
  def md5(text: String) = digest("md5", text)
  def sha256(text: String) = digest("sha-256", text)

  val ampEscape = Pattern.compile("&(?!(?:[a-zA-Z]+|(?:#[0-9]+|#[xX][0-9a-fA-F]+));)")

  /*! The `wrapHtml` and `unwrapHtml` are special helpers for converting
  text for use inside HTML inputs, where all occurrences of &, <, >, ", '
  must be replaced with their entity reference escapes.

  These methods are idempotent, so they can be safely used with `addSetter`
  (see [[core/src/main/scala/model.scala]]):

  ``` {.scala}
  wrapHtml(wrapHtml(...(wrapHtml(input)) == wrapHtml(input)
  unwrapHtml(unwrapHtml(...(unwrapHtml(input)) == unwrapHtml(input)
  ```

  These methods should be used with any case of persisting text input for web
  (e.g. into records in ORM, attributes and text elements in XML, etc.)
  */
  def wrapHtml(s: String) = ampEscape.matcher(s)
      .replaceAll("&amp;")
      .replaceAll(">", "&gt;")
      .replaceAll("<", "&lt;")
      .replaceAll("\"", "&quot;")
      .replaceAll("\'", "&#x27;")
      .replaceAll("\\r\\n|\\r", "\n")

  def unwrapHtml(s: String) = s
      .replaceAll("&gt;", ">")
      .replaceAll("&#x3[eE];", ">")
      .replaceAll("&#62;", ">")
      .replaceAll("&lt;", "<")
      .replaceAll("&#x3[cC];", "<")
      .replaceAll("&#60;", "<")
      .replaceAll("&quot;", "\"")
      .replaceAll("&#x22;", "\"")
      .replaceAll("&#34;", "\"")
      .replaceAll("&apos;", "\'")
      .replaceAll("&#x27;", "\'")
      .replaceAll("&#39;", "\'")
      .replaceAll("&amp;", "&")
      .replaceAll("&#x26;", "&")
      .replaceAll("&#38;", "&")
      .replaceAll("\\r\\n|\\r", "\n")


  /*! ## Context DSL

  If you import package object `core` you can use special syntax for
  working with context:

  ``` {.scala}
  'variable := value
  ```
  */
  @inline implicit def symbol2contextVarHelper(sym: Symbol): ContextVarHelper =
    new ContextVarHelper(sym)

}
