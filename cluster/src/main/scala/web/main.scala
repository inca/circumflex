package pro.savant.circumflex
package cluster

import core._, web._, freemarker._
import pro.savant.circumflex.security.SecurityRouter

class Main extends Router {
  new SecurityRouter(auth)

  'conf := conf

  get("/") = ftl("/index.ftl")

  get("/locale.js") = {
    serveLastModified(msg.lastModified)
    response.contentType("application/javascript")
    ftl("/locale.js.ftl")
  }

  sub("/auth") = new AuthRouter

  auth.require()

}

class AuthRouter extends Router {

  get("/login") = ftl("/auth/login.ftl")

  post("/login") = partial {
    val l = param("l").trim
    val p = param("p").trim
    val rememberMe = param("r") == "true"
    conf.users.children
        .find(_.name == l)
        .filter(_.passwdSha256 == sha256(p))
        .map { u =>
      auth.login(u, rememberMe)
      'redirect := auth.returnLocation
    } getOrElse {
      notices.addError("login.failed")
    }
  }

  get("/logout") = {
    auth.logout()
    auth.returnLocation
  }

}