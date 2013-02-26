package pro.savant.circumflex
package security

import core._, web._
import collection.mutable._

/*!# Single Sign-On

Some cross-domain authentication routines require that regular HTTP sessions,
managed on single domains, are logically united with special identifier
referred to as _SSO id_.

The `SsoManager` trait provides an interface for storing and retrieving
SSO ids with their last access timestamps.

Each time the session-based authentication occurs, the session is checked
for validity by retrieving the `ssoId` and ensuring that it is not expired.

The `sso.session.timeout` configuration parameter defines the timeout
in milliseconds after which all sessions which share the same `ssoId` become
invalid. The default timeout is 5 minutes.

Typical implementations will write last access timestamps into a two-columned
table (`ssoId` and `lastAccessTime`) shared by all application nodes.
*/
trait SsoManager {

  private val _timeout = cx.getLong("sso.session.timeout")
      .getOrElse(5 * 60 * 1000l)
  def timeout: Long = _timeout

  def ssoIdOption = sessionOption.flatMap(_.getString("cx.auth.ssoId"))

  def ssoId = ssoIdOption.get

  def registerSession(ssoId: String) {
    sessionOption.map { sess =>
      sess += "cx.auth.ssoId" -> ssoId
      updateLastAccessTime(ssoId)
    }
  }

  def touchCurrentSession() {
    ssoIdOption.map(sid => updateLastAccessTime(sid))
  }

  def checkCurrentSession: Boolean = ssoIdOption
      .flatMap(sid => getLastAccessTime(sid))
      .map(time => time + timeout > System.currentTimeMillis)
      .getOrElse(false)

  def invalidateCurrentSession() {
    ssoIdOption.map(sid => remove(sid))
    sessionOption.map(_.invalidate())
  }

  /*! Following methods must be implemented:

  * `updateLastAccessTime(ssoId)` writes current timestamp (in milliseconds)
    as the last access time of specified `ssoId`;
  * `getLastAccessTime(ssoId)` returns previously memorized last access time
    of specified `ssoId`, or returns `None` if it is not found;
  * `remove(ssoId)` deletes last access time information associated with
    specified `ssoId`;

  */

  def updateLastAccessTime(ssoId: String)

  def getLastAccessTime(ssoId: String): Option[Long]

  def remove(ssoId: String)

}

/*! ## Default implementation

`DefaultSsoManager` keeps all SSO ids in hashmap. In production scenario with
 multiple nodes you will have to implement some sort of persistent SSO manager
 to provide your application with SSO.
*/
object DefaultSsoManager extends SsoManager {

  val hash = new HashMap[String, Long]

  def updateLastAccessTime(ssoId: String) {
    hash += ssoId -> System.currentTimeMillis
  }

  def getLastAccessTime(ssoId: String) = hash.get(ssoId)

  def remove(ssoId: String) {
    hash -= ssoId
  }

}

/*! ## SSO Excpetion

`SsoException` is a simple marker for SSO-related exceptions.
They are considered as non-critical and are subject for forcing the principal logout.
*/
class SsoException(msg: String) extends Exception(msg)