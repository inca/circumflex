package ru.circumflex.core

import java.io.File

/**
 * Represents an HTTP header for `xSendFile` method of `RequestRouter`. X-SendFile feature
 * allows sending files via Web server, thus eliminating all the dirty work from the
 * application code. See the documentation of your Web server to obtain information on
 * how to configure X-SendFile feature.
 */
trait XSendFileHeader {
  def name: String
  def value(f: File): String
}

/**
 * Default `XSendFileHeader` implementation works with most Web browsers,
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
