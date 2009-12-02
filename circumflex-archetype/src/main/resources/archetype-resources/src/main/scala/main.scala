#set( $symbol_pound = '#' )
#set( $symbol_dollar = '$' )
#set( $symbol_escape = '\' )

package ${package}

import ru.circumflex.core.RequestRouter
import ru.circumflex.freemarker.FreemarkerHelper
import java.text.SimpleDateFormat
import java.util.Date

class Main extends RequestRouter
    with FreemarkerHelper {

  ctx += "currentYear" -> new SimpleDateFormat("yyyy").format(new Date)
  ctx += "host" -> header("Host").getOrElse("localhost")

  get("/") = ftl("index.ftl")

}