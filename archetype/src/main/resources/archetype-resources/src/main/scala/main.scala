#set( $symbol_pound = '#' )
#set( $symbol_dollar = '$' )
#set( $symbol_escape = '\' )
package ${package}

import circumflex._, core._, web._, freemarker._
import java.util.Date

class Main extends Router {
  val log = new Logger("${package}")

  'currentDate := new Date

  get("/test") = "I'm fine, thanks!"
  get("/") = ftl("index.ftl")

}