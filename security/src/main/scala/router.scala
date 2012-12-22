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

  // Try to authenticate the request from session
  auth.doSessionAuth()

  // Try SSO login (this also strips away SSO parameters, if any.
  auth.trySsoLogin()

  // Now try secure routing
  if (host == auth.secureDomain) {

    /*! The default scheme as returned by `web.scheme` method is overriden.

    You can find this useful for composing URLs for all static resources
    served from the secure domain. */
    'scheme := auth.secureScheme

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
      else sendRedirect(auth.createSsoUrl(auth.principal, location))
    }

    // TODO implement `/sso-check` along with `requireAuth`

  }

}