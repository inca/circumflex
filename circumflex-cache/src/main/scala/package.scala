package ru.circumflex

import core._
import net.sf.ehcache.CacheManager

package object cache {
  val CACHE_LOG = new Logger("ru.circumflex.cache")
  val ehcacheManager = cx.instantiate[CacheManager](
    "cx.ehcacheManager", CacheManager.create())
}