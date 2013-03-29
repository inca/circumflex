package pro.savant.circumflex
package nm

import core._, web._, freemarker._

class ApplicationRouter extends Router {
  get("/?") = {
    'applications := Application.all.sortBy(_.title())
    ftl("/applications/index.ftl")
  }

  get("/~new") = ftl("/applications/new.ftl")

  post("/?") = {
    val name = param("n").trim
    val title = param("t").trim
    val m = regex.name.matcher(name)
    if (!m.matches()) {
      notices.addError("app.name.incorrect")
      sendRedirect("/applications/~new")
    }
    if (title.isEmpty) {
      notices.addError("app.title.empty")
      sendRedirect("/applications/~new")
    }

    Application.findByName(name) match {
      case Some(a) =>
        notices.addError("app.exists")
        sendRedirect("/applications/~new")
      case _ =>
    }

    val app = new Application(name)
    app.title := title
    app.save()
    notices.addInfo("app.saved")
    sendRedirect("/applications/" + name)
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