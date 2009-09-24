package myapp

import javax.servlet.http.HttpServletRequest
import circumflex.core._

/**
 * Author: incarnate
 * Date: Aug 30, 2009
 * Time: 4: 12: 01 PM
 */

class MainRouter(request: HttpServletRequest, config: Config) extends RequestRouter(request, config) {

  get("/([^/]+)/to/([^/]+)".r, headers("Host" -> "(.*)".r)) = ftl("myapp/hello.ftl")

}
