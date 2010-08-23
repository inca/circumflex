package ru.circumflex.web

import javax.servlet.http.HttpServletRequest
import collection.immutable.Map

/*!# HTTP Request

The `HttpRequest` class wraps specified `raw` HTTP servlet request and allows you to
use core Scala classes to operate with Servlet API. This class is designed to efficiently
cover all methods of `HttpServletRequest`, so we strongly discourage you from accessing
the `raw` field.

*/

class HttpRequest(val raw: HttpServletRequest) {

  trait AuthType
  object BasicAuth extends AuthType
  object ClientCertificateAuth extends AuthType
  object DigestAuth extends AuthType
  object FormAuth extends AuthType
  case class CustomAuth(scheme: String) extends AuthType
  object NoAuth extends AuthType

  /*!# The initialization of each member can be a bit of effort, mostly because
  of incompatibilities between Java and Scala API for collections.
  So we use lazy vals to initialize them on demand.
  */

  lazy val authType: AuthType = raw.getAuthType match {
    case null => NoAuth
    case HttpServletRequest.BASIC_AUTH => BasicAuth
    case HttpServletRequest.CLIENT_CERT_AUTH => ClientCertificateAuth
    case HttpServletRequest.DIGEST_AUTH => DigestAuth
    case HttpServletRequest.FORM_AUTH => FormAuth
    case s: String => CustomAuth(s)
  }

  


}