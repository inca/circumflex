package ru.circumflex.site

import core.RequestRouter
import freemarker.FreemarkerHelper

class Main extends RequestRouter
    with FreemarkerHelper {

  get("/") = ftl("index.ftl")

}