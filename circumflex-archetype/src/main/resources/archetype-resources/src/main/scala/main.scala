#set( $symbol_pound = '#' )
#set( $symbol_dollar = '$' )
#set( $symbol_escape = '\' )
package ${package}

import ru.circumflex.core._
import ru.circumflex.web._
import ru.circumflex.freemarker.FTL._
import java.util.Date

class Main extends RequestRouter {
  val log = new Logger("${package}")

  'currentDate := new Date

  get("/") = ftl("index.ftl")

}