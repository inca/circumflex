package ru.circumflex.md

import java.io.{BufferedReader, InputStreamReader}

object MD {
  def apply(): String = {
    var text = ""
    val reader = new BufferedReader(new InputStreamReader(this.getClass.getResourceAsStream("/test.md")))
    try {
      var s = reader.readLine
      while (s != null) {
        text += s + "\n"
        s = reader.readLine
      }
      text = Markdown(text)
    } finally {
      reader.close
    }
    return text
  }

  def print() = println(apply())

  def calcTime(times: Int) = {
    val t = System.currentTimeMillis
    (0 to times).foreach(i => apply())
    println(System.currentTimeMillis - t)
  }
}