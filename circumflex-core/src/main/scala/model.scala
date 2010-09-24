package ru.circumflex.core

/*!# Data model support

To make Circumflex components independent from various view technologies
we introduce some basic interfaces here. Different components implement
these interfaces while view technologies should provide proper support
for them.
*/
trait Wrapper[T] {
  def item: T
}