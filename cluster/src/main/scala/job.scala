package pro.savant.circumflex
package cluster

import java.io.File

class ModuleBuildJob(val cluster: Cluster)
    extends ProcessMonitor {

  def key = "module-mci"

  def builder = cluster.project.mvn("clean", "install")

}

class ProjectBuildJob(val cluster: Cluster)
    extends ProcessMonitor {

  def key = "project-mci"

  def builder = cluster.project.rootProject.mvn("clean", "install")

}

class BuildClusterJob(val cluster: Cluster)
    extends Monitor {

  def key = "build-cluster"

  def process() {
    if (!cluster.classesDir.isDirectory) {
      println("Full project rebuild required.")
      new ProjectBuildJob(cluster).execute().join()
    }
    cluster.servers.children.foreach { s =>
      new BuildServerJob(s).execute().join()
    }
    println("Cluster build complete.")
  }

}

class BuildServerJob(val server: Server)
    extends Monitor {

  def key = "build-server"

  def process() {
    println("================================")
    println("Building " + server)
    println("================================")
    server.copyClasses()
    server.copyDependencies()
    server.children.foreach { node =>
      println(">>>> Node " + node.name())
      node.copyResources()
      node.saveProperties()
      node.buildJar()
    }
  }
}

class DeployServerJob(val server: Server,
                      val backup: Boolean = false)
    extends ProcessMonitor {

  protected def suffix = if (backup) "backup" else "main"

  def key = "deploy-server-" + suffix

  def sourceDir: File = if (backup) server.backupDir else server.mainDir

  def targetDir: File = new File(server.dir())

  def location: String = server.location + ":" + targetDir.getPath

  def builder = new ProcessBuilder(
    "rsync", "-vrlptzh",
    "-e", "ssh",
    sourceDir.getCanonicalPath,
    location)

}


class RestartServerJob(server: Server)
    extends Monitor {

  def key = "restart-server"

  def process() {
    println("===================================")
    println("Redeploying on " + server)
    println("===================================")
    println(">>>> main -> " + server)
    server.children.filter(!_.isBackup).foreach(_.remote.stop())
    new DeployServerJob(server, false).execute().join()
    server.children.filter(!_.isBackup).foreach(_.remote.run())
    println("==== Allowing nodes to startup ====")
    Thread.sleep(6000)
    server.children.filter(!_.isBackup).foreach { node =>
      try {
        node.remote.checkOnline_!()
      } catch {
        case e: Exception =>
          println(e.getMessage, "error")
          return
      }
    }
    println("====== Main nodes are online ======")
    println(">>>> backup -> " + server)
    server.children.filter(_.isBackup).foreach(_.remote.stop())
    new DeployServerJob(server, true).execute().join()
    server.children.filter(_.isBackup).foreach(_.remote.run())
  }

}

class RestartClusterJob(cluster: Cluster)
    extends Monitor {

  def key = "restart-cluster"

  def process() {
    cluster.servers.children.foreach { server =>
      new RestartServerJob(server)
    }
  }

}

