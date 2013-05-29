package circumflex

import core._

package object security {

  val SECURITY_LOG = new Logger("circumflex.security")

  lazy val isSslEnabled = cx.getBoolean("cx.ssl").getOrElse(false)

}
