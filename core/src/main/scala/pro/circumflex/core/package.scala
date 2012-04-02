package pro.circumflex

import org.slf4j._

/*!# Circumflex Core Module

Circumflex Core contains basic API used by another modules.

Package object `core` contains following members:

  * `cx` provides access to Circumflex configuration parameters (see `CircumflexCfg`)
  * `ctx` acquires current context from `ContextManager` singleton

## DSL implicits

To take advantage of context reading/writing DSL (`'key := value` and
`'key.get`) you should import the implicit conversions from package object
`core`:

``` {.example}
import pro.circumflex._, core._
'key := value
```
*/
package object core {
  protected[core] val CX_LOG = LoggerFactory.getLogger("pro.circumflex.core")

  val cx = CircumflexCfg
  def ctx = ContextManager.get

  @inline implicit def sym2helper(sym: Symbol) = new ContextVarHelper(sym)
}