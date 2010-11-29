package ru.circumflex

import java.util.Random
import java.util.regex.Pattern

package object me {
  val chars = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
  val keySize = 20
  val rnd = new Random

  object regexes {
    val normalizeLines = Pattern.compile("(?:\\r\\n|\\r||\\n) *")
    val blocks = Pattern.compile("(?<=\\n\\n|\\A\\n*)" +
        "( {0,3}([*>|]) +(?s:.+?)(?:\\Z|\\n{2,}(?!\\2|\\s))|" +
        " {0,3}(\\d\\.) +(?s:.+?)(?:\\Z|\\n{2,}(?!(?:\\d\\.)|\\s))|" +
        "(?s:.+?)(?=\\Z|\\n{2,}))")
    val outdent = Pattern.compile("^ {1,4}", Pattern.MULTILINE)
    val htmlNameExpr = "[a-z][a-z0-9\\-_:.]*?\\b"
    val inlineHtmlStart = Pattern.compile("^<(" + htmlNameExpr + ").*?(/)?>",
      Pattern.MULTILINE | Pattern.CASE_INSENSITIVE | Pattern.DOTALL)
  }
}