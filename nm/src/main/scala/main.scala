package pro.savant.circumflex
package nm

import core._, web._, freemarker._

class Main extends Router {

  get("/") = auth.principalOption match {
    case Some(u) => ftl("/index.ftl")
    case _ => sendRedirect("/login")
  }

  get("/*.html") = ftl("/pages/" + uri(1) + ".ftl")

  get("/login") = ftl("/login.ftl")

  post("/login") = sendError(501)

  get("/logout") = sendError(501)

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

