package ru.circumflex

import java.util.Random
import java.util.regex.Pattern

package object me {
  val chars = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
  val keySize = 20
  val rnd = new Random

  object regexes {
    val normalizeLines = Pattern.compile("(?:\\r\\n|\\r|\\n) *")
    val blocks = Pattern.compile("\\n{2,}")
    val outdent = Pattern.compile("^ {1,4}", Pattern.MULTILINE)
    val htmlNameExpr = "[a-z][a-z0-9\\-_:.]*?\\b"
    val inlineHtmlStart = Pattern.compile("^<(" + htmlNameExpr + ").*?(/)?>",
      Pattern.MULTILINE | Pattern.CASE_INSENSITIVE | Pattern.DOTALL)
    val linkDefinition = Pattern.compile("^ {0,3}\\[(.+?)\\]: *(\\S.*?)" +
        "(\\n *\"(.+?)\")?(?=\\n+|\\Z)", Pattern.MULTILINE)
    val blockSelector = Pattern.compile("(?<=\\A.*?) *\\{(\\#[a-z0-9_-]+)?((?:\\.[a-z0-9_-]+)+)?\\}(?=\\Z|\\n)", 
      Pattern.CASE_INSENSITIVE)

    // deterministic patterns

    val d_ol = Pattern.compile("^\\d\\. +.*", Pattern.DOTALL)
    val d_table = Pattern.compile("^\\-{3}.*\\n\\|?-{3,}$", Pattern.DOTALL)
    val d_heading = Pattern.compile("^\\#{1,6} .*$", Pattern.DOTALL)
    val d_code = Pattern.compile("(?: {4,}.*\\n?)+", Pattern.MULTILINE)
  }
}