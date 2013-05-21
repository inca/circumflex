package circumflex
package core

import cache._
import java.io._
import collection.immutable.Map
import java.util.concurrent.locks.ReentrantLock
import java.util.{Properties, Enumeration => JEnum}
import collection.JavaConversions._

/*! `PropertyFile` provides means to read Java `.properties` into
Scala-friendly immutable map. Use this with `CacheCell` to make files
hot-reloadable, or update manually with `load` method. */
class PropertiesFile(val file: File)
    extends CachedFile
    with Map[String, String] {

  def cachedFile = file

  protected val _lock = new ReentrantLock

  protected var _map = Map[String, String]()

  def load() {
    _lock.lock()
    try {
      val props = new Properties
      val is = new FileInputStream(file)
      val buf = new BufferedInputStream(is)
      try {
        props.load(buf)
      } catch {
        case e: Exception =>
          CX_LOG.error("Error reading " + file.getAbsolutePath, e)
          None
      } finally {
        is.close()
        buf.close()
      }
      _map = props.keys()
          .asInstanceOf[JEnum[String]]
          .map(k => k -> props.getProperty(k))
          .toMap
    } finally {
      _lock.unlock()
      // Also mark as clean
      reclaim()
    }
  }

  def get(key: String) = _map.get(key)

  def iterator = _map.iterator

  def +[B1 >: String](kv: (String, B1)) = _map + kv

  def -(key: String) = _map - key

  // Load by default
  load()

}
