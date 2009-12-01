package ru.circumflex.site

import core.RequestRouter
import freemarker.FreemarkerHelper
import java.text.SimpleDateFormat
import java.util.Date

class Main extends RequestRouter
    with FreemarkerHelper {

  ctx += "currentYear" -> new SimpleDateFormat("yyyy").format(new Date)
  ctx += "host" -> header("Host").getOrElse("localhost")

  get("/") = ftl("index.ftl")

}