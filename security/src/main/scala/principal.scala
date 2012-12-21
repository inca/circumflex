package pro.savant.circumflex
package security

import core._

/*! # Principal

The `Principal` trait should be implemented in your application
by the `User` class (or whatever abstraction you use for authentication).

Typically you would use an ORM library to provide methods for storing and
retrieving users, but your application may choose different technology
for managing authentication data. The `Principal` trait in this sense is
agnosting to storage technology. However, the authentication architecture
provided by Circumflex
*/
trait Principal {

  /*! A unique ID is used to lookup the user from the storage and to associate
  it with session on current domain. The `uniqueId` method should return string
  representation (for use in cookies). The same string is to be used in the
  implementation of method `lookup` in `Auth`.
   */
  def uniqueId: String

  /*! The `checkSession`, `registerSession` and `purgeSessions` methods
  are used in SSO to ensure that the logout occurs on all domains bound
  to the application. This implies that session ids are stored along with `Principal`
  (probably in a separate table).

  The `locationId` parameter is a random string generated at `authBase` which
  is used to logically group sessions on different domains opened from
  the same user agent (which could mean the same machine, same location, etc.).

  The `checkSession` should return `true` if the current session is associated
   with the principal at specified `locationId`.

   The `registerSession` should associate the id of current session with specified
  `locationId` for the principal.

  The `purgeSession` removes all associations of session ids with specified `locationId`
  for the principal.
  */

  def checkSession(locationId: String): Boolean

  def registerSession(locationId: String)

  def purgeSessions(locationId: String)

}

/*! ## Stubs

Circumflex Auth includes default implementations of `Principal` which
are used by `NoAuth` stub.
*/

class DummyPrincipal extends Principal {

  def uniqueId = "dummy"

  def checkSession(locationId: String) = false

  def registerSession(locationId: String) {}

  def purgeSessions(locationId: String) {}

}

object DummyPrincipal extends DummyPrincipal

