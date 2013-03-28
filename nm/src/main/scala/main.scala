package pro.savant.circumflex
package nm

import core._, web._, freemarker._, security._

class Main extends Router {

  new SecurityRouter(auth)

  get("/") = auth.principalOption match {
    case Some(u) => ftl("/index.ftl")
    case _ => sendRedirect("/login")
  }

  get("/login") = ftl("/login.ftl")

  post("/login") = {
    val l = param("l").trim
    val p = param("p").trim
    val rememberMe = param("r") == "true"

    val u = auth.children
        .find(_.name == l)
        .filter(_.password == p).getOrElse {
      notices.addError("login.failed")
      sendRedirect("/login")
    }
    auth.login(u, rememberMe)

    sendRedirect("/")
  }

  get("/logout") = {
    auth.logout()
    sendRedirect("/")
  }

  auth.require()


  sub("/applications") = {

    get("/?") = {
      'applications := Application.all
      ftl("/applications/index.ftl")
    }

    get("/~new") = ftl("/applications/new.ftl")

    post("/?") = {
      // TODO create application
      sendError(501)
    }

    sub("/:name") = {

      val application = Application
          .findByName(param("name"))
          .getOrElse(sendError(404))
      'application := application

      get("/?") = ftl("/applications/view.ftl")

      get("/~edit") = ftl("/applications/edit.ftl")

      post("/?") = {
        // TODO edit application
        sendError(501)
      }

      get("/~delete") = ftl("/application/delete.ftl")

      delete("/?") = {
        // TODO delete application
        sendError(501)
      }

    }

  }
}

