package ru.circumflex

import java.util.Random
import java.util.regex.Pattern

package object me {
  val chars = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
  val keySize = 20
  val rnd = new Random

  object regexes {
    val normalizeLines = Pattern.compile("(?:\\r\\n|\\r||\\n) *")
    val blocks = Pattern.compile("(?<=\\n\\n|\\A\\n*)( {0,3}([*>|]|\\d+\\.) +" +
        "(?s:.+?)(?:\\Z|\\n{2,}(?!\\2|\\s))|(?s:.+?)(?=\\Z|\\n{2,}))")
    val outdent = Pattern.compile("^ {1,4}", Pattern.MULTILINE)
  }
}