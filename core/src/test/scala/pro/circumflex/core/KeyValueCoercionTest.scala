package pro.circumflex.core

import org.specs2.mutable.SpecificationWithJUnit
import collection.mutable.HashMap
import java.text.SimpleDateFormat

class KeyValueCoercionTest extends SpecificationWithJUnit {

  val m = new HashMap[String, Any] with KeyValueCoercion
  m += "str" -> "Hello world"
  m += "int" -> 96
  m += "int_s" -> "96"
  m += "long" -> 75l
  m += "long_s" -> "75"
  m += "short" -> 9
  m += "short_s" -> "9"
  m += "float" -> 3.14f
  m += "float_s" -> "3.14"
  m += "double" -> 2.71d
  m += "double_s" -> "2.71"
  m += "boolean" -> true
  m += "boolean_s" -> "false"
  m += "date" -> "29.01.1988"
  m += "timestamp" -> "1988-01-29 20:16:05 +0500"

  "KeyValueCoercion mixed into map" should {
    "coerce values to String" in {
      m.getString("str") must_== Some("Hello world")
      m.getString("nop") must_== None
      m.getString("boolean") must_== Some("true")
    }
    "safely coerce values to Int" in {
      m.getInt("int") must_== Some(96)
      m.getInt("int_s") must_== Some(96)
      m.getInt("str") must_== None
    }
    "safely coerce values to Long" in {
      m.getLong("long") must_== Some(75l)
      m.getLong("long_s") must_== Some(75l)
      m.getLong("str") must_== None
    }
    "safely coerce values to Short" in {
      m.getShort("short") must_== Some(9.toShort)
      m.getShort("short_s") must_== Some(9.toShort)
      m.getShort("str") must_== None
    }
    "safely coerce values to Float" in {
      m.getFloat("float") must_== Some(3.14f)
      m.getFloat("float_s") must_== Some(3.14f)
      m.getFloat("str") must_== None
    }
    "safely coerce values to Double" in {
      m.getDouble("double") must_== Some(2.71d)
      m.getDouble("double_s") must_== Some(2.71d)
      m.getDouble("str") must_== None
    }
    "safely coerce values to BigDecimal" in {
      m.getBigDecimal("double") must_== Some(BigDecimal(2.71d))
      m.getBigDecimal("double_s") must_== Some(BigDecimal(2.71d))
      m.getBigDecimal("str") must_== None
    }
    "safely coerce values to Boolean" in {
      m.getBoolean("boolean") must_== Some(true)
      m.getBoolean("boolean_s") must_== Some(false)
      m.getBoolean("str") must_== None
    }
    "safely coerce values to Date (with or without pattern)" in {
      m.getDate("date", "dd.MM.yyyy").map(_.getTime) must_==
          Some(new SimpleDateFormat("dd.MM.yyyy").parse("29.01.1988").getTime)
      m.getDate("date", "yyyy-MM-dd HH:mm:ss") must_== None
      m.getDate("str", "dd.MM.yyyy") must_== None
      m.getTimestamp("timestamp").map(_.getTime) must_==
          Some(570467765000l)
    }
  }

}
