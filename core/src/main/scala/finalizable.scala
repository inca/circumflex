package pro.savant.circumflex
package core

/*! `Finalizable` trait adds abstract finalization functionality
based on collecting functions `() => Unit` in current context.

The implementation then decides when to perform such finalization
(i.e. execute all the functions).
*/
trait Finalizable {

  protected def baseName = "finalizers"

  def finalizers = ctx.getAs[Seq[() => Unit]](baseName).getOrElse(Nil)

  def enqueueFinalizer(key: String, fn: () => Unit) {
    if (!ctx.contains(baseName + "." + key)) {
      ctx.update(baseName + "." + key, true)
      ctx.update(baseName, finalizers ++ Seq(fn))
    }
  }

  def pushFinalizer(key: String, fn: () => Unit) {
    if (!ctx.contains(baseName + "." + key)) {
      ctx.update(baseName + "." + key, true)
      ctx.update(baseName, Seq(fn) ++ finalizers)
    }
  }

  def doFinalize() {
    finalizers.foreach(_.apply())
  }

}