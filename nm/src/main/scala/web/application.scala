package pro.savant.circumflex
package nm

import core._, web._, freemarker._
import org.apache.commons.io.FileUtils

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
      val name = param("n").trim
      val title = param("t").trim
      val m = regex.name.matcher(name)
      if (!m.matches()) {
        notices.addError("app.name.incorrect")
        sendRedirect(prefix)
      }
      if (title.isEmpty) {
        notices.addError("app.title.empty")
        sendRedirect(prefix)
      }

      Application.findByName(name)
          .filter(_.name != application.name) match {
        case Some(a) =>
          notices.addError("app.exists")
          sendRedirect(prefix)
        case _ =>
      }
      val app = if (application.name != name) {
        FileUtils.deleteQuietly(application.descriptorFile)
        new Application(name)
      } else application
      app.title := title
      app.save()
      notices.addInfo("app.saved")
      sendRedirect("/applications/" + app.name)
    }

    get("/~delete") = ftl("/applications/delete.ftl")

    delete("/?") = {
      FileUtils.deleteQuietly(application.descriptorFile)
      notices.addInfo("app.deleted")
      sendRedirect("/applications")
    }

  }
}