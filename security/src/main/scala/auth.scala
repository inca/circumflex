package pro.savant.circumflex
package security

import core._, web._

/*! The `Auth` trait maintains basic authentication management logic.

In order to use authentication in your application you should implement
this trait specifying the `Principal` implementation (e.g. `User`)
as type parameter.
*/
trait Auth[U <: Principal] {

  /*! Principal is resolved from context or from session under key `cx.principal`. */
  val KEY = "cx.principal"

  /*! The `lookup` method is used to find a `Principal` designated by type parameter
  `U` by provided unique identifier (see [[auth/src/main/scala/principal.scala]]).
   */
  def lookup(principalId: String): Option[U]

  /*! The `anonymous` method should return a constant instance of unauthenticated
  user. Best practice is to provide a stable transient singleton which overrides
  all methods of your implementation to deny any state updates.
  */
  def anonymous: U

  /*! The `authError` method is used to abort current execution
  with authentication exception. Default implementation sends
  HTTP 403 Access Denied, but you can override this method to implement
  custom authentication error processing logic.
  */
  def authError(message: String): Nothing = sendError(403, message)

  /*! The `principalOption` method resolves current principal by first looking up
  the context and then falling back to session lookup.
   */
  def principalOption: Option[U] = ctx.getAs[U](KEY)
      .orElse(sessionOption.flatMap(
    _.getString(KEY).flatMap(id => lookup(id))))

  /*! The `principalOrAnonymous`, as the name implies, performs the lookup and
  returns the anonymous identity as returned by `anonymous` in the case of failure.*/
  def principalOrAnonymous = principalOption.getOrElse(anonymous)

  /*! The `principal` method returns currently authenticated principal or aborts
  the execution with `authError`.

  It should only be used in contexts where authentication is mandatory. */
  def principal = principalOption.getOrElse(
    authError("No authentication information available."))

}

/*! ## The `NoAuth` stub

If auth is not configured in your application, the `NoAuth` is used
as main authentication manager with `DummyPrincipal` as principal implementation.
*/
object NoAuth extends Auth[DummyPrincipal] {

  def anonymous = DummyPrincipal

  def lookup(principalId: String) = None
}
