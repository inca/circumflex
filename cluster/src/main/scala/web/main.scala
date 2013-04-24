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

  sub("/cluster/([^/]+)".r) = {

    val cluster = getCluster(uri(1)).getOrElse(sendError(404))
    'cluster := cluster
    val status = pro.savant.circumflex.cluster.status.get(cluster)
    'status := status

    get("/?") = ftl("/cluster/view.ftl")

    get("/~job-progress").and(request.isXHR) = {
      var outFrom = param.getInt("outFrom").getOrElse(0)
      var errFrom = param.getInt("errFrom").getOrElse(0)
      val lines = status.currentJob.map { job =>
        val outLines = job.outLines.drop(outFrom)
        outFrom += outLines.size
        val errLines = job.errLines.drop(errFrom)
        errFrom += errLines.size
        outLines ++ errLines
      } getOrElse Nil
      'lines := lines
      'outFrom := outFrom
      'errFrom := errFrom
      ftl("/cluster/job-progress.p.ftl")
    }

    post("/~module-mci") = partial {
      val pm = cluster.project.mvn("clean", "compile")
      val job = new Job("module-mci", pm)
      status.runJob(job)
      'redirect := prefix
    }

    post("/~root-mci") = partial {
      val pm = cluster.project.rootProject.mvn("clean", "compile")
      val job = new Job("root-mci", pm)
      status.runJob(job)
      'redirect := prefix
    }

  }

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