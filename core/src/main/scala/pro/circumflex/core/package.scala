package pro.circumflex

import org.slf4j._

/*!# Circumflex Core Module

Circumflex Core contains basic API used by another modules.
*/

package object core {
  protected[core] val CX_LOG = LoggerFactory.getLogger("pro.circumflex.core")

  val cx = CircumflexCfg
}