#set( $symbol_pound = '#' )
#set( $symbol_dollar = '$' )
#set( $symbol_escape = '\' )
package ${package}

import ru.circumflex.core._
import ru.circumflex.web._
import ru.circumflex.freemarker.FTL._
import java.text.SimpleDateFormat
import java.util.Date
import org.slf4j.LoggerFactory

class Main extends RequestRouter {

  val log = new Logger("${package}")

  'currentYear := new SimpleDateFormat("yyyy").format(new Date)
  'host := header('Host).getOrElse("localhost")

  get("/") = ftl("index.ftl")

}