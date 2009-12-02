package ${groupId}

import java.text.SimpleDateFormat
import java.util.Date
import ru.circumflex.core._
import ru.circumflex.freemarker._

class Main extends RequestRouter
           with FreemarkerHelper {

  ctx += "currentYear" -> new SimpleDateFormat("yyyy").format(new Date)
  ctx += "host" -> header("Host").getOrElse("localhost")

  get("/") = ftl("index.ftl")

}