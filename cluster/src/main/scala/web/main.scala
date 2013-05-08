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

    val baseUrl = prefix
    'baseUrl := baseUrl

    get("/?") = ftl("/cluster/view.ftl")

    get("/~job-progress").and(request.isXHR) = {
      var from = param.getInt("from").getOrElse(0)
      val lines = status.lastJob.map { job =>
        val lines = job.output.drop(from)
        from += lines.size
        lines
      } getOrElse Nil
      'lines := lines
      'from := from
      ftl("/cluster/job-progress.p.ftl")
    }

    post("/~module-mci") = partial {
      status.runJob(new ModuleBuildJob(cluster))
      'redirect := baseUrl
    }

    post("/~project-mci") = partial {
      status.runJob(new ProjectBuildJob(cluster))
      'redirect := baseUrl
    }

    post("/~build") = partial {
      status.runJob(new BuildClusterJob(cluster))
      'redirect := baseUrl
    }

    post("/~restart") = partial {
      status.runJob(new RestartClusterJob(cluster))
      'redirect := baseUrl
    }

    sub("/server/:uuid") = {

      val server = cluster.servers
         .find(_.shortUuid == param("uuid"))
          .getOrElse(sendError(404))
      'server := server

      post("/~build") = partial {
        status.runJob(new BuildServerJob(server))
        'redirect := baseUrl
      }

      post("/~deploy-main") = partial {
        status.runJob(new DeployServerJob(server, false))
        'redirect := baseUrl
      }

      post("/~deploy-backup") = partial {
        status.runJob(new DeployServerJob(server, true))
        'redirect := baseUrl
      }

      post("/~restart") = partial {
        status.runJob(new RestartServerJob(server))
        'redirect := baseUrl
      }

    }

    sub("/node/:uuid") = {

      val node = cluster.nodes
          .find(_.shortUuid == param("uuid"))
          .getOrElse(sendError(404))
      'node := node

      get("/~status").and(request.isXHR) = {
        'pid := node.remote.getPid
        ftl("/node/status.p.ftl")
      }

      post("/~run") = partial {
        node.remote.run()
        'redirect := baseUrl
      }

      post("/~stop") = partial {
        node.remote.stop()
        'redirect := baseUrl
      }

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