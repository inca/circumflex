package ru.circumflex.web

import java.io.File

/*!# X-SendFile feature

Most modern web servers provide the `X-SendFile` feature: they take control over
the process of sending a file to the client. Your application, for example, may
check permissions, set some headers, like `Content-Disposition`, and then delegate
the hard stuff to web server.

Refer to the documentation of your favorite web server for more information.
 */

/**
 * Represents an HTTP header for `xSendFile` method of `RequestRouter`. X-SendFile feature
 * allows sending files via Web server, thus eliminating all the dirty work from the
 * application code. See the documentation of your Web server to obtain information on
 * how to configure X-SendFile feature.
 *
 * For more information refer to
 * <a href="http://circumflex.ru/api/2.0.1/circumflex-web/xsendfile.scala">xsendfile.scala</a>.
 */
trait XSendFileHeader {
  def name: String
  def value(f: File): String
}

/**
 * Default `XSendFileHeader` implementation works with most Web servers,
 * including Apache HTTPD and lighttpd.
 */
object DefaultXSendFileHeader extends XSendFileHeader {
  def name = "X-SendFile"
  def value(f: File): String = f.getAbsolutePath
}

/**
 * Nginx implementation needs to know URI instead of file path, so we leave the
 * implementation details up to you.
 */
trait NginxXSendFileHeader extends XSendFileHeader {
  def name = "X-Accel-Redirect"
}
