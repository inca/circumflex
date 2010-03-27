package ru.circumflex.md

import java.io.{BufferedReader, InputStreamReader}

object MD {
  def apply() = {
    var text = ""
    val reader = new BufferedReader(new InputStreamReader(this.getClass.getResourceAsStream("/test.md")))
    try {
      var s = reader.readLine
      while (s != null) {
        text += s + "\n"
        s = reader.readLine
      }
      println(Markdown(text))
    } finally {
      reader.close
    }
  }
}