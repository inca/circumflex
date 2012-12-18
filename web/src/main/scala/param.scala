package pro.savant.circumflex
package web

import core._

import collection.Iterator
import collection.mutable.Map

/*!# Parameters

  The `param` object of is a convenient helper which is used to
  retrieve the parameters of current match or current request:

    * the parameters are first resolved from `MatchResult` objects found in context;

    * if no match result contain a parameter with specified name,
  then the result is searched in request parameters.

  In other words, match results always override request parameters.
  */
object param extends Map[String, String] with KeyValueCoercion {

  def +=(kv: (String, String)): this.type = this

  def -=(key: String): this.type = this

  def iterator: Iterator[(String, String)] = ctx.iterator.flatMap(p => p._2 match {
    case m: MatchResult => m.params.iterator
    case s: String => Seq(p._1 -> s).iterator
    case _ => Iterator.empty
  }) ++ requestOption.toSeq.flatMap(_.params).iterator

  def get(key: String): Option[String] = iterator.find(_._1 == key).map(_._2)

  override def default(key: String): String = ""

  /*! Use the `list` method to retrieve multi-value parameters from request. */
  def list(key: String): Seq[String] = iterator.filter(_._1 == key).map(_._2).toList
}