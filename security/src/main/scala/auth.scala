package pro.savant.circumflex
package security

import core._, web._

/*! # Authentication API

The `Auth` trait maintains basic authentication management logic.

In order to use authentication in your application you should implement
this trait specifying the `Principal` implementation (e.g. `User`)
as type parameter.
*/
trait Auth[U <: Principal] {

  /*! Principal is resolved from context or from session under key `cx.principal`. */
  val KEY = "cx.principal"

  /*! The `authError` method is used to abort current execution
  with authentication exception. Default implementation sends
  HTTP 403 Access Denied, but you can override this method to implement
  custom authentication error processing logic.
  */
  def authError(message: String): Nothing = sendError(403, message)

  /*! The `lookup` method is used to find a `Principal` designated by type parameter
  `U` by provided unique identifier (see [[auth/src/main/scala/principal.scala]]).
   */
  def lookup(principalId: String): Option[U]

  /*! The `anonymous` method should return a constant instance of unauthenticated
  user. Best practice is to provide a stable transient singleton which overrides
  all methods of your implementation to deny any state updates.

  It is important that the `anonymous` method returns equal instances
  (singletons are preferred).
  */
  def anonymous: U

  /*! The `secureDomain` method returns the domain name of your application
  which will host authentication-related routes and cookies
  (e.g. `secure.myapp.com`).

  The separate domain is required by SSO architecture
  (see [[security/src/main/scala/sso.scala]]).
  */
  def secureDomain: String

  /*! The `secureScheme` method should return `https` for all production scenarios.
  You can override it to return `http` for development scenario (without SSL
  certificate).
  */
  def secureScheme: String = "https"

  /*! The `isSecure` method returns `true` if `secureScheme` is `https`. */
  def isSecure = secureScheme == "https"

  /*! ## Sessions and locations

  Current HTTP specifications do not allow cookies from one domain to be passed
  to another domains (due to security reasons). This fact makes SSO very
  difficult to implement.

  In order to keep track of different sessions on different domains for the same
  user we introduce the `locationId` method -- a random string generated
  at login time and placed in every session.
  */
  def locationId = sessionOption.flatMap(_.getString("cx.auth.location"))

  /*! ## Retrieving authenticated principal */

  /*! The `principalOption` method resolves current principal by first looking up
  the context and then falling back to session lookup.
   */
  def principalOption: Option[U] = ctx.getAs[U](KEY)
      .orElse(sessionOption.flatMap(
    _.getString(KEY).flatMap(id => lookup(id))))
      .filter(_ != anonymous)

  /*! The `principalOrAnonymous`, as the name implies, performs the lookup and
  returns the anonymous identity as returned by `anonymous` in the case of failure.*/
  def principalOrAnonymous = principalOption.getOrElse(anonymous)

  /*! The `principal` method returns currently authenticated principal or aborts
  the execution with `authError`.

  It should only be used in contexts where authentication is mandatory
  (and probably checked before). */
  def principal = principalOption.getOrElse(
    authError("No authentication information available."))

  /*! ## Setting authentication */

  /*! The `set` method associates specified `principal` with the current context
  authentication. It is implied that all subsequent calls to `principalOption`
  and other authentication retrieval methods will return `specified` principal,
  but only *within the same context*.

  This method affects only context, all other authentication facilities
  (session, cookies, etc.) remain unchanged.*/
  def set(principal: U) {
    if (principal == anonymous)
      ctx -= KEY
    else ctx += KEY -> principal
  }

  /*! The `login` method establishes the authentication for current user session
  across all application domains (if you use SSO) and, optionally, sets the
  "remember-me" cookie.

  This method must only be invoked on the secure domain as specified by
  the `secureDomain` method. */
  def login(principal: U, rememberMe: Boolean) {
    principalOption.filter(_.uniqueId != principal.uniqueId).map { u =>
    // Another principal is logged in. Let's log him off.
      locationId.map(lid => u.purgeSessions(lid))
      dropRememberMeCookie()
    }
    // Log the principal in with new locationId
    val loc = randomString(8)
    session += KEY -> principal
    session += "cx.auth.location" -> loc
    principal.registerSession(loc)
    if (rememberMe)
      setRememberMeCookie()
    else dropRememberMeCookie()
  }

  /*! The `logout` method removes all authentication information from
  context, session and cookies, purges all sessions of the same location
  (which will cause the principal to logout from all domains) and invalidates
  current session.

  This method must only be invoked on the secure domain as specified by
  the `secureDomain` method. */
  def logout() {
    principalOption.map { u =>
      locationId.map(lid => u.purgeSessions(lid))
    }
    ctx -= KEY
    sessionOption.map(_.invalidate())
    dropRememberMeCookie()
  }

  /*! ## Security tokens

  Security tokens are used to pass and validate authentication information
  about principal by digesting it with the `secret` obtained from the principal
  and random `nonce`.

  The `mkToken` method returns a digested token for specified `principal` with
  specified `nonce`.
  */
  def mkToken(principal: U, nonce: String) = principal.uniqueId + ":" + nonce +
      ":" + sha256(principal.uniqueId + ":" + nonce + ":" + principal.secret)

  /*! The `parseToken` is a counterpart of `mkToken`, which returns parses the
  specified `token`, validates it and returns the principal in case of success. */
  def parseToken(token: String): Option[U] = try {
    // Read unique id
    val i1 = token.indexOf(":")
    val id = token.substring(0, i1)
    // Read nonce
    val i2 = token.indexOf(":", i1 + 1)
    val nonce = token.substring(i1 + 1, i2)
    // Read secret
    val secret = token.substring(i2 + 1)
    // Lookup principal by id
    lookup(id) match {
      case Some(u) =>
        val correctSecret = sha256(u.uniqueId + ":" + nonce + ":" + u.secret)
        if (secret != correctSecret) {
          SECURITY_LOG.debug("Token secret mismatch: " + token)
          None
        } else Some(u)
      case _ =>
        SECURITY_LOG.debug("Principal not found by id: " + id)
        None
    }
  } catch {
    case e: Exception =>
      SECURITY_LOG.debug("Malformed token: " + token)
      None
  }

  /*! ## Remember me cookies

  Remember-me cookies are set on the secure domain as specified by
  the `secureDomain` method. They allow to retain authentication information
  across different sessions in time.

  The cookies are set by the `setRememberMeCookie` method and dropped
  by the `dropRememberMeCookie` method.

  The `doRememberMeAuth()` tries to perform authentication using remember-me
  cookies. In case of any failure the remember-me cookieas are dropped.
  The `login` method is invoked in case of success.
  */
  def setRememberMeCookie() {
    responseOption.map { resp =>
      resp.cookies += HttpCookie(
        name = "ea-auth",
        value = mkToken(principal, randomString(4)),
        path = "/auth",
        maxAge = 365 * 24 * 60 * 60, // 1 year
        secure = isSecure)
    }
  }

  def dropRememberMeCookie() {
    response.cookies += HttpCookie(
      name = "ea-auth",
      value = "",
      path = "/auth",
      maxAge = 0,
      secure = isSecure)
  }

  def doRememberMeAuth() {
    requestOption.flatMap(_.cookies.find(_.name == "cx-auth")).map { cookie =>
      parseToken(cookie.value) match {
        case Some(u) =>
          login(u, true)
        case _ =>
          dropRememberMeCookie()
      }
    }
  }


}

/*! ## The `NoAuth` stub

If auth is not configured in your application, the `NoAuth` is used
as main authentication manager with `DummyPrincipal` as principal implementation.
*/
object NoAuth extends Auth[DummyPrincipal] {

  def anonymous = DummyPrincipal

  def lookup(principalId: String) = None

  def secureDomain = "localhost"
}
