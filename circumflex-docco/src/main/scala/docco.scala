/*
 * Copyright (C) 2009-2010 Boris Okunskiy (http://incarnate.ru)
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

package ru.circumflex.docco

import java.io.{FileReader, BufferedReader, File}

class Docco(val file: File) {
  var sections: Seq[Pair[String, String]] = Nil

  val commentBegin = "^\\s*/\\*\\*?\\s*(.*)".r
  val commentEnd = "^\\s*(.*?)\\*/\\s*".r
  val commentSingleLine = "^\\s*//\\s*(.*)".r
  val commentSingleBlock = "^\\s*/\\*\\s*(.*?)\\*/\\s*".r
  val commentBody = "^\\s*\\*?\\s*(.*)".r

  def empty_?(str: String) = str == null || str.trim == ""

  def parse = {
    val reader = new BufferedReader(new FileReader(file))
    try {
      var insideComment = false
      var code = ""
      var comment = ""
      var str = reader.readLine

      def flushSection() = {
        sections ++= List(comment.trim -> code.trim)
        code = ""
        comment = ""
      }

      while (str != null) {
        str match {
          case commentSingleLine(s) =>
            if (!empty_?(code)) flushSection
            if (!empty_?(s)) comment += s + "\n"
          case commentSingleBlock(s) =>
            if (!empty_?(code)) flushSection
            if (!empty_?(s)) comment += s + "\n"
          case commentBegin(s) =>
            if (!empty_?(code) || !empty_?(comment)) flushSection
            insideComment = true
            comment += s + "\n"
          case commentEnd(s) if insideComment =>
            if (!empty_?(comment)) comment += s + "\n"
            insideComment = false
          case commentBody(s) if insideComment =>
            comment += s + "\n"
          case _ =>
            code += str + "\n"
        }
        str = reader.readLine
      }
      if (!empty_?(code) || !empty_?(comment))
        flushSection
    } finally {
      reader.close()
    }
  }

}
