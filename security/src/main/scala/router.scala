package pro.savant.circumflex
package security

import core._, web._

/*! # Security router

The `SecurityRouter` must be executed at the start of the main routing of
 your web application.

It contains routes which establish session and cookie-based authentication,
as well as SSO functionality.

*/
class SecurityRouter[U <: Principal](val auth: Auth[U])
  extends Router {

  'auth := auth

  // Memorize the `returnTo` across requests
  param.get("returnTo").map { loc =>
    flash.update("cx.auth.returnTo", loc)
  }

  // Try to authenticate the request from session
  auth.doSessionAuth()

  // Try SSO login (this also strips away SSO parameters, if any
  auth.trySsoLogin()

  // Now try secure routing
  if (host == auth.secureDomain) {

    // Check the auth cookies
    if (auth.isEmpty)
      auth.doRememberMeAuth()

    /*! The default scheme as returned by `web.scheme` method is overriden.

    You can find this useful for composing URLs for all static resources
    served from the secure domain. */
    ctx.update("cx.scheme", auth.secureScheme)

    /*! This script will force client to go through series of redirect hops
    to obtain SSO authentication from the secure domain. It should be included
    on different domains *only for anonymous (unauthenticated) users*.
    */
    get("/auth/sso.js") = {
      response.contentType("application/javascript")
      send(auth.getSsoJsResponse)
    }

    /*! This route appends the security parameters to the location specified
     in the `to` request parameter, if the user is authenticated, and redirects
     him back.
    */
    get("/auth/sso-return") = {
      val location = auth.returnLocation
      if (auth.isEmpty)
        sendRedirect(location)
      else sendRedirect(auth.createSsoUrl(location))
    }

    /*! This route appends the security parameters to the location specified
     in the `to` request parameter, if the user is authenticated, and redirects
     him back. Unlike `sso-return`, the user is redirected to the `loginUrl`
     of the `auth` if no authentication exist.
    */
    get("/auth/sso-check") = {
      val location = auth.returnLocation
      if (auth.isEmpty)
        sendRedirect(appendParam(auth.loginUrl, "returnTo", location))
      else sendRedirect(auth.createSsoUrl(location))
    }

    get("/auth/sso-check.js") = {
      val location = auth.returnLocation
      if (auth.isEmpty)
        sendJsRedirect(appendParam(auth.loginUrl, "returnTo", location))
      else sendJsRedirect(auth.createSsoUrl(location))
    }

  }

}