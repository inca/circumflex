#set( $symbol_pound = '#' )
#set( $symbol_dollar = '$' )
#set( $symbol_escape = '\' )
package ${package}

import _root_.ru.circumflex.core.RequestRouter
import _root_.ru.circumflex.freemarker.FreemarkerHelper
import _root_.java.text.SimpleDateFormat
import _root_.java.util.Date
import _root_.org.slf4j.LoggerFactory

class Main extends RequestRouter
    with FreemarkerHelper {

  val log = LoggerFactory.getLogger("${package}")

  ctx += "currentYear" -> new SimpleDateFormat("yyyy").format(new Date)
  ctx += "host" -> header("Host").getOrElse("localhost")

  get("/") = ftl("index.ftl")

}