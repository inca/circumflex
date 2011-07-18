package ru.circumflex
package web
import java.io.File

/*!# X-SendFile feature

Most modern web servers provide the `X-SendFile` feature: they take control over
the process of sending a file to the client. Your application, for example, may
check permissions, set some headers, like `Content-Disposition`, and then delegate
the hard stuff to web server.

Refer to the documentation of your favorite web server for more information.
 */
trait XSendFileHeader {
  def name: String
  def value(f: File): String
}

object DefaultXSendFileHeader extends XSendFileHeader {
  def name = "X-SendFile"
  def value(f: File): String = f.getAbsolutePath
}

trait NginxXSendFileHeader extends XSendFileHeader {
  def name = "X-Accel-Redirect"
}
