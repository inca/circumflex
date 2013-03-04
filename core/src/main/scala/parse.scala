package pro.savant.circumflex
package core

import java.text.SimpleDateFormat
import java.util.Date

/*! # Parsing Scalars

Circumflex Core includes tiny helper which helps safely parsing
common types from string, swallowing external exceptions by accepting
input as by-name parameters
(e.g. `parse.long(None.asInstanceOf[Option[String]].get)` will return `None`
insead of throwing an exception.

Methods are self-explanatory, so are not documented.
*/
object parse {

  def date(str: => String): Date = dateOption(str).get
  def dateOption(str: => String): Option[Date] = dateOption("yyyy-MM-dd", str)

  def date(fmt: String, str: => String): Date = dateOption(fmt, str).get
  def dateOption(fmt: String, str: => String): Option[Date] = try {
    Some(new SimpleDateFormat(fmt).parse(str))
  } catch {
    case e: Exception => None
  }

  def time(str: => String): Date = timeOption(str).get
  def timeOption(str: => String): Option[Date] = dateOption("yyyy-MM-dd", str)
      .orElse(parse.dateOption("yyyy-MM-dd HH:mm:ss", str))
      .orElse(parse.dateOption("yyyy-MM-dd HH:mm:ss ZZ", str))

  def int(str: => String): Int = intOption(str).get
  def intOption(str: => String): Option[Int] = try {
    Some(str.trim.toInt)
  } catch {
    case e: Exception => None
  }

  def long(str: => String): Long = longOption(str).get
  def longOption(str: => String): Option[Long] = try {
    Some(str.trim.toLong)
  } catch {
    case e: Exception => None
  }

  def float(str: => String): Float = floatOption(str).get
  def floatOption(str: => String): Option[Float] = try {
    Some(str.trim.toFloat)
  } catch {
    case e: Exception => None
  }

  def double(str: => String): Double = doubleOption(str).get
  def doubleOption(str: => String): Option[Double] = try {
    Some(str.trim.toDouble)
  } catch {
    case e: Exception => None
  }

  def boolean(str: => String): Boolean = booleanOption(str).get
  def booleanOption(str: => String): Option[Boolean] = try {
    Some(str.trim.toBoolean)
  } catch {
    case e: Exception => None
  }

  def bigDecimal(str: => String): BigDecimal = bigDecimalOption(str).get
  def bigDecimalOption(str: => String): Option[BigDecimal] = try {
    Some(BigDecimal(str.trim.replaceAll(",", ".")))
  } catch {
    case e: Exception => None
  }

}