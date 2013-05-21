package pro.savant.circumflex
package security

import java.io.Serializable

/*! # Principal

The `Principal` trait should be implemented in your application
by the `User` class (or whatever abstraction you use for authentication).

Typically you would use an ORM library to provide methods for storing and
retrieving users, but your application may choose different technology
for managing authentication data. The `Principal` trait in this sense is
agnostic to storage technology.
*/
trait Principal extends Serializable {

  /*! A unique ID is used to lookup the user from the storage and to associate
  it with session on current domain. The `uniqueId` method should return string
  representation (for use in cookies). The same string is to be used in the
  implementation of method `lookup` in `Auth`.
   */
  def uniqueId: String

  /*! The `secret` method should return a string containing the secret information
  (e.g. SHA256 of user password) which could be used to check the authenticity of
  the principal. */
  def secret: String

}

/*! ## Stubs

Circumflex Auth includes default implementations of `Principal` which
are used by `NoAuth` stub.
*/

class DummyPrincipal extends Principal {

  def uniqueId = "dummy"

  def secret = ""

}

object DummyPrincipal extends DummyPrincipal

