package circumflex
package cluster

import java.io._
import core._, xml._, cache._
import scala.collection.mutable.HashMap
import collection.JavaConversions._
import java.util.regex.Pattern
import java.util.{Date, Properties}
import java.util.jar._
import org.apache.commons.io.IOUtils
import scala.sys.process._
import java.net.{InetSocketAddress, Socket}

class Cluster(val project: Project)
    extends StructHolder
    with XmlFile { cluster =>

  def elemName = "cluster"

  def status = circumflex.cluster.status.get(cluster)

  def baseDir = project.baseDir

  val clusterDir = new File(baseDir, "src/cluster")

  def descriptorFile = new File(clusterDir, "cluster.xml")

  val _id = attr("id")
  def id = _id.getOrElse("")

  val _mainClass = text("main-class")
  def mainClass = _mainClass.getOrElse(
    throw new IllegalArgumentException("<main-class> tag not specified."))

  val resources = new ClusterResources(this)

  val _servers = new ListHolder[Server] {

    def elemName = "servers"

    def read = {
      case "server" => new Server(cluster)
    }

  }

  def servers = _servers.children.toSeq

  def getServer(id: String) = servers.find(_.id == id)

  def server(id: String) = getServer(id).get

  def nodes: Seq[Node] = servers.flatMap(_.nodes)

  def getNode(id: String) = {
    var _id = id
    if (_id.startsWith(cluster.id + "-"))
      _id = _id.substring(cluster.id.length + 1)
    nodes.find(_.toString == cluster.id + "-" + _id)
  }

  def node(id: String) = getNode(id).get

  override def toString = id

  val classesDir = new File(baseDir, "target/classes")
  val dependencyDir = new File(baseDir, "target/lib")
  val targetDir = new File(baseDir, "target/cluster")
  val workDir = new File(targetDir, "work")

  def mainCxProps = new PropsFile(new File(classesDir, "cx.properties"))

  def classesTimestamp: Option[Date] = {
    val f = new File(baseDir, "target/classes.timestamp")
    if (f.isFile)
      Some(new Date(f.lastModified))
    else None
  }

  load()
}

class ClusterResources(val cluster: Cluster)
    extends StructHolder { res =>

  def elemName = "resources"

  val _dir = attr("dir")
  val dir = new File(cluster.clusterDir, _dir.getOrElse("resources"))

  val _filter = attr("filter")
  val filterPattern = try {
    Pattern.compile(_filter())
  } catch {
    case e: Exception =>
      DEFAULT_FILTER_PATTERN
  }

}

class Server(val cluster: Cluster)
    extends StructHolder { server =>

  def elemName = "server"

  def project = cluster.project

  val _id = attr("id")
  def id = _id.get.orElse(address.get).getOrElse("")

  val address = attr("address")

  val _user = attr("user")
  def user = _user.getOrElse(System.getProperty("user.name"))

  val dir = attr("dir")

  val _jvmArgs = attr("jvm-args")
  def jvmArgs = _jvmArgs.getOrElse("-Xms256m -Xmx2g")

  val _nodes = new ListHolder[Node] {

    def elemName = "nodes"

    def read = {
      case "node" => new Node(server)
    }

  }

  def nodes = _nodes.children.toSeq

  def getNode(name: String) = nodes.find(_.name() == name)

  def node(name: String) = getNode(name).get

  val _tasks = new ListHolder[ServerTask] {

    def elemName = "tasks"

    def read = {
      case "copy" => new CopyTask(server)
      case "lessc" => new LesscTask(server)
    }

  }

  def tasks = _tasks.children.toSeq

  val _props = new PropsHolder("properties", project.baseDir)

  def properties: Map[String, String] = {
    val result = new HashMap[String, String]
    result ++= project.properties
    result ++= cluster.mainCxProps.toMap
    result += "node.address" -> server.address()
    result ++= _props.toMap
    result.toMap
  }

  /*! Local directories to perform configuration, packaging and installation.*/
  def workDir = cluster.workDir
  def targetDir = new File(cluster.targetDir, address())

  def mainDir = new File(targetDir, "main")
  def mainLibDir = new File(mainDir, "lib")

  def backupDir = new File(targetDir, "backup")
  def backupLibDir = new File(backupDir, "lib")

  /*! Dependencies (as acquired by executing `mvn dependency:copy-dependencies`)
  are copied twice: in `target/cluster/:server/main/lib` and in
  `target/cluster/:server/backup/lib`.
  */
  def copyDependencies() {
    val libDir = cluster.dependencyDir
    if (!libDir.isDirectory) {
      currentMonitor.println("Copying dependencies from Maven.")
      project.mvn(
        "dependency:copy-dependencies",
        "-DoutputDirectory=" + libDir.getCanonicalPath
      ).start().waitFor()
      currentMonitor.println("Dependencies copied from Maven Repository.")
    }
    new FileCopy(libDir, mainLibDir).copyIfNewer()
    new FileCopy(libDir, backupLibDir).copyIfNewer()
  }

  /*! Classes are copied to work directory to perform node JAR
  processing and packaging. */
  def copyClasses() {
    new FileCopy(cluster.classesDir, workDir).copyIfNewer()
  }

  def uuid = sha256(toString)

  def shortUuid = uuid.substring(0, 8)

  def location = user + "@" + address()

  override def toString = location

}

class Node(val server: Server)
    extends PropsHolder("node", server.project.baseDir) { node =>

  def cluster = server.cluster

  def project = cluster.project

  val name = attr("name")

  val backup = attr("backup")

  def isBackup = backup.getOrElse("false") == "true"

  def workDir = cluster.workDir

  def rootDir = if (isBackup) server.backupDir else server.mainDir

  def libDir = new File(rootDir, "lib")

  def jarFile = new File(rootDir, toString + "-" + shortUuid + ".jar")

  /*! ### Node properties evaluation

  Each node evaluates its set of properties (which are subsequently
  written into `cx.properties` of each assembled jar) by
  reading them from following locations in specified order:

    * project POM, including its ancestors, if any;
    * `cx.properties` in `target/classes`;
    * server properties in `src/cluster/cluster.xml` of corresponding node.
    * node properties in `src/cluster/cluster.xml` of corresponding node.

  Additionally, every property with key starting with `include.` are
  resolved as property files.

  Properties from each step will overwrite the onces defined earlier.

  Some special properties are added for each node:

    * `node.address` specifies the server address of the node (typically, the
      internal address is used);

    * `node.name` specifies the name of this node;

    * `node.backup` specifies, if the node is marked backup;

    * `node.uuid` resolves to SHA256 hash of following string:

      ```
      ${cluster.id}:${server.address}:${node.name}
      ```

    * `node.shortUuid` is 8 first letters of `node.uuid`.

  */
  def properties: Map[String, String] = {
    val result = new HashMap[String, String]
    result ++= server.properties
    result ++= this.toMap
    result += "node.name" -> this.name()
    result += "node.backup" -> this.isBackup.toString
    result += "node.uuid" -> this.uuid
    result += "node.shortUuid" -> this.shortUuid
    result.toMap
  }

  /*! Cluster resources are copied to work directory with filtering
  during build.
  */
  def copyResources() {
    new FileCopy(cluster.resources.dir, workDir)
        .filteredCopy(cluster.resources.filterPattern, properties)
  }

  /*! Properties are saved to `cx.properties` at work directory during build. */
  def saveProperties() {
    val out = new FileOutputStream(new File(workDir, "cx.properties"))
    try {
      val _p = new Properties
      _p.putAll(mapAsJavaMap(properties))
      _p.store(out, this.toString)
    } finally {
      out.close()
    }
  }

  /*! Jar must be built only if all resources, properties are copied to work
  directory and dependencies are inplace. */
  def buildJar() {
    val f = jarFile
    currentMonitor.println("Building " + f.getName)
    val out = new FileOutputStream(f)
    val jar = new JarOutputStream(out, prepareManifest)
    try {
      jar.setLevel(9)
      addToJar(jar, workDir)
    } finally {
      jar.close()
      out.close()
    }
    currentMonitor.println("Sucessfully built " + f.getName)
  }

  protected def addToJar(jar: JarOutputStream, file: File) {
    val prefix = workDir.getCanonicalPath
    val path = file.getCanonicalPath
    if (!path.startsWith(prefix))
      return
    val relPath = path.substring(prefix.length)
        .replaceAll("^/|/$", "")
    if (file.isDirectory) {
      if (relPath != "") {
        val e = new JarEntry(relPath + "/")
        jar.putNextEntry(e)
        jar.closeEntry()
      }
      file.listFiles.foreach(f => addToJar(jar, f))
    } else {
      val e = new JarEntry(relPath)
      e.setTime(file.lastModified)
      jar.putNextEntry(e)
      val fin = new FileInputStream(file)
      try {
        IOUtils.copy(fin, jar)
      } finally {
        fin.close()
        jar.closeEntry()
      }
    }
  }

  protected def prepareManifest: Manifest = {
    val libPrefix = libDir.getCanonicalPath
    val paths = libDir.listFiles.flatMap { f =>
      val p = f.getCanonicalPath
      if (p.startsWith(libPrefix))
        Some("lib/" + p.substring(libPrefix.length).replaceAll("^/", ""))
      else None
    }
    val manifest = new Manifest
    manifest.getMainAttributes.put(Attributes.Name.MANIFEST_VERSION, "1.0")
    manifest.getMainAttributes.put(Attributes.Name.MAIN_CLASS, cluster.mainClass)
    manifest.getMainAttributes.put(Attributes.Name.CLASS_PATH, paths.mkString(" "))
    manifest
  }

  def jarBuiltDate = new Date(jarFile.lastModified)

  def isJarBuilt = jarFile.isFile

  def isJarUpToDate = cluster.classesTimestamp
      .map(t => isJarBuilt && jarBuiltDate.getTime > t.getTime)
      .getOrElse(false)

  def classifier = if (isBackup) "backup" else "main"

  /*! Remote commands are executed via `ssh`. */
  object remote {

    /*! Process PID is discovered by issuing `ps` command via `ssh`. */
    def getPid: Option[String] =
      Seq("ssh", server.location, "-C", "ps -A -o pid,args")
          .lines_!
          .find(_.contains(shortUuid))
          .map(_.trim)
          .flatMap { line =>
        val i = line.indexOf(" ")
        if (i == -1) None
        else Some(line.substring(0, i))
      }

    def location = new File(server.dir(), classifier + "/" + jarFile.getName).getPath

    def runCommand = "nohup java -jar " + location + " " +
        server.jvmArgs + " > /dev/null 2>&1 &"

    def run() {
      getPid match {
        case Some(_) =>
          currentMonitor.println("Node " + node.toString + " already running.", "error")
        case _ =>
          currentMonitor.println("Starting node " + node.toString)
          val p = new java.lang.ProcessBuilder("ssh", server.location, "-C", runCommand)
              .start()
          p.getInputStream.close()
          p.getOutputStream.close()
          p.getErrorStream.close()
      }
    }

    def stop() {
      getPid match {
        case Some(pid) =>
          currentMonitor.println("Stopping node " + node.toString)
          new java.lang.ProcessBuilder("ssh", server.location, "-C", "kill " + pid)
              .start()
              .waitFor()
        case _ =>
          currentMonitor.println("Node " + node.toString + " is not running.", "error")
      }
    }

    def port: Int = parse.intOption(node.properties("cx.port"))
        .getOrElse(throw new IllegalStateException(
      "Node port unspecified (set `cx.port` property)."))

    /*! The `checkOnline_!` method attempts to bind to remote socket
    to ensure that the node is up. It performs 10 bind attempts every 6 seconds
    (60 seconds totally) and throws an exception indicating that node startup
    failed. Exits quietly if node is online.
    */
    def checkOnline_!() {
      (0 until 10).foreach { i =>
        currentMonitor.println(
          "Trying to connect to " + node.toString + " (" + (i + 1) + ")")
        val sock = new Socket()
        try {
          sock.connect(new InetSocketAddress(server.address(), port), 6000)
          currentMonitor.println("Node " + node.toString + " is online.")
          return
        } catch {
          case e: Exception =>
        } finally {
          sock.close()
        }
        Thread.sleep(6000)
      }
      throw new IllegalStateException("Node " + node.toString + " did not start.")
    }

  }

  def uuid = sha256(toString)

  def shortUuid = uuid.substring(0, 8)

  override def toString = cluster.id + "-" + server.address() + "-" + name()

}
