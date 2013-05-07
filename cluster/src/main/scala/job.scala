package pro.savant.circumflex
package cluster

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
