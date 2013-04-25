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

