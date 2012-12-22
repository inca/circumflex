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

/*! The `returnLocationOption` returns an URL for redirecting the user
    after successful authentication. The URL is resolved from request parameter,
    falling back to the `Referer` header.*/
  def returnLocationOption = param.get("returnTo")
    .orElse(request.headers.get("Referer"))
    .filter(!_.startsWith(secureUrlPrefix + "/auth"))

  /*! The `returnLocation` also falls back to the `defaultReturnLocation` as
  specified in this configuration. */
  def returnLocation = returnLocationOption.getOrElse(defaultReturnLocation)

  def defaultReturnLocation: String

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

  The separate domain is required by the [SSO architecture](#sso)
  of Circumflex Security.
  */
  def secureDomain: String

  /*! The `secureScheme` method should return `https` for all production scenarios.
  You can override it to return `http` for development scenario (without SSL
  certificate).
  */
  def secureScheme: String = "https"

  /*! The `isSecure` method returns `true` if `secureScheme` is `https`. */
  def isSecure = secureScheme == "https"

  /*! The `secureUrlPrefix` method just concatenates `secureScheme`
  and `secureDomain` to form secure URL prefix.
  */
  def secureUrlPrefix = secureScheme + "://" + secureDomain

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

  /*! The `principalOption` method resolves current principal from the context.
   */
  def principalOption: Option[U] = ctx.getAs[U](KEY).filter(_ != anonymous)

  /*! The `principalOrAnonymous`, as the name implies, performs the lookup and
  returns the anonymous identity as returned by `anonymous` in the case of failure.*/
  def principalOrAnonymous = principalOption.getOrElse(anonymous)

  /*! The `principal` method returns currently authenticated principal or aborts
  the execution with `authError`.

  It should only be used in contexts where authentication is mandatory
  (and probably checked before). */
  def principal = principalOption.getOrElse(
    authError("No authentication information available."))

  /*! The `isEmpty` method returns true if no authentication is
  associated with current context. */
  def isEmpty = principalOption.isEmpty

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

  /*! The `setSessionAuth` associates specified `principal` with
  current session and registers this session with specified `locationId`. */
  def setSessionAuth(principal: U, locationId: String) {
    sessionOption.map { sess =>
      sess += KEY -> principal
      sess += "cx.auth.location" -> locationId
      principal.registerSession(locationId)
    }
  }

  /*! The `doSessionAuth` tries to authenticate current context using
  the session. */
  def doSessionAuth() {
    sessionOption.map { sess =>
      try {
        val id = sess.getString(KEY).get
        val principal = lookup(id).get
        val lid = locationId.get
        if (!principal.checkSession(lid))
          throw new IllegalStateException
        set(principal)
      } catch {
        case e: Exception =>
          locationId.map(lid => principalOption.map(_.purgeSessions(lid)))
          sess.invalidate()
      }
    }
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
    setSessionAuth(principal, loc)
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

  /*! ## Single Sign-On (SSO)  {#sso}

  Single Sign-On is a combined technique which enables your application to pass
  authentication data between different domains.

  SSO requires both client and server logic to overcome the limitations of current
  HTTP specs which do not allow cookies from one domain to be passed to another
  (due to obvious security implications).

  ### Architecture

  Here's a brief description of SSO architecture introduced by Circumflex Security:

  1. Let's assume that your application runs on `myapp.com` and provides
     different services to authenticated users
     on two more domains: `myservice1.com` and `myservice2.com`.

  2. Let's assume that user Alice is trying to access `myapp.com` the first time.
     Her session at `myapp.com` does not contain authentication information, so
     she the anonymous page is displayed to her.

  3. Then Alice logs into `myapp.com`. The login is implemented on the different
     domain `secure.myapp.com` with SSL enabled to prevent all sorts of
     man-in-the-middle attacks.

  4. The authentication information of Alice now becomes associated with
     the session at `secure.myapp.com`.

  5. When Alice is returned back to `myapp.com`, however, her authentication
     will not be recognized by `myapp.com`, because she still has the old session.

     This is when the client side of SSO begins its work. The anonymous page
     served to Alice will include a small javascript from
     `https://secure.myapp.com/auth/sso.js`. As you can see, the script is
     served from the secure domain where the authentication data exists.

  6. The SSO script forces the browser to go through a series of redirect hops
     in order to pass authentication from the secure domain to `myapp.com`.

     These hops include:

     * `https://secure.myapp.com/auth/sso_return?returnTo=http://myapp.com/` will
       append security parameters to the original URL (`http://myapp.com`)
       and redirect the user there;
     * `http://myapp.com?sso=<security_parameters>` — the parameters are validated
       for authenticity at `myapp.com` and session authentication is established
       on that domain; the user is redirected to the original URL once again
       with all security parameters stripped.

  7. Two other domains `myservice1.com` and `myservice2.com` behave in exactly
     same way.

  */

  /*! ## SSO API

  SSO relies on passing security information from domain to domain using
  HTTP redirects and SHA-256 digests.

  The `createSsoUrl` is used to append security parameters of specified `principal`
  to the specified `url`.

  When the user accesses this URL the security parameters will be used
  to establish his session authentication on that domain.

  The `timeout` parameter specifies the validity period of this URL.
  */
  def createSsoUrl(principal: U, url: String, timeout: Long = 60000l) = {
    val nonce = randomString(8)
    val deadline = System.currentTimeMillis + timeout
    val lid = locationId.getOrElse(randomString(8))
    val token = sha256(mkToken(principal, nonce) +
        ":" + deadline.toString + ":" + lid)
    var result = url
    val i = url.indexOf("?")
    if (i == -1)
      result += "?"
    else result += "&"
    result + "sso=token&sso_nonce=" + nonce +
        "&sso_deadline=" + deadline +
        "&sso_location=" + lid +
        "&sso_principal=" + principal.uniqueId +
        "&sso_token=" + token
  }

  /*! The `trySsoLogin` method scans current request for SSO security parameters
  and tries to authenticate current session by looking up supplied principal
  and checking his authenticity. */
  def trySsoLogin() {
    if (param("sso") == "token") {
      val id = param("sso_principal")
      lookup(id).map { principal =>
        val token = param("sso_token")
        val nonce = param("sso_nonce")
        val lid = param("sso_location")
        val deadline = parse.longOption(param("sso_deadline")).getOrElse(0l)
        val correctToken = sha256(mkToken(principal, nonce) +
            ":" + deadline.toString + ":" + lid)
        if (correctToken == token && System.currentTimeMillis <= deadline) {
          setSessionAuth(principal, lid)
        }
      }
    }
    // Drop SSO params from query string
    if (param.contains("sso")) {
      var uri = web.origin + request.originalUri
      val qs = request.queryString.replaceAll("&?sso[^=]*=[^&]*", "")
      if (qs != "")
        uri += "?" + qs
      sendRedirect(encodeURI(uri))
    }
  }

  /*! The `getSsoJsResponse` will return a Javascript redirect code
  to `<secureBase>/auth/sso-return`. This step is described in SSO architecture.*/
  def getSsoJsResponse: String = principalOption.map { u =>
    val pf = secureUrlPrefix + "/auth/sso-return?returnTo="
    "window.location.replace(\"" + escapeJs(pf) +
        "\" + encodeURIComponent(window.location.href));"
  }.getOrElse("")

}

/*! ## The `NoAuth` stub

If auth is not configured in your application, the `NoAuth` is used
as main authentication manager with `DummyPrincipal` as principal implementation.
*/
object NoAuth extends Auth[DummyPrincipal] {

  def anonymous = DummyPrincipal

  def lookup(principalId: String) = None

  def secureDomain = "localhost"

  def defaultReturnLocation = "http://localhost/"
}
