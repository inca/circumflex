package pro.savant.circumflex

import core._
import net.sf.ehcache.CacheManager

package object cache {

  val CACHE_LOG = new Logger("pro.savant.circumflex.cache")

  val ehcacheManager = cx.instantiate[CacheManager](
    "cx.ehcacheManager", CacheManager.create())

}