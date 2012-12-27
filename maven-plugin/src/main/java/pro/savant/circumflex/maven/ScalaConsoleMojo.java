package pro.savant.circumflex.maven;

import freemarker.template.Configuration;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.apache.maven.artifact.Artifact;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;

import pro.savant.circumflex.core.Circumflex;
import pro.savant.circumflex.web.StandaloneServer;
import pro.savant.circumflex.scalaconsole.DefaultScalaConsoleRouter;

import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.net.URI;
import java.net.URL;
import java.util.Enumeration;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;

import java.awt.Desktop;

/**
 * @goal console
 * @requiresDependencyResolution complie+runtime+test
 */
public class ScalaConsoleMojo extends AbstractCircumflexMojo {

  /**
   * @parameter default-value="10888"
   */
  protected Integer port;

  /**
   * @parameter default-value="target/scalaconsole-work"
   */
  File tempRoot;

  // Embedded Jetty Server
  StandaloneServer server;

  // Shutdown hook
  Thread shutdownHook = new Thread("cx.scalaconsole.shutdownHook") {

    public void run() {
      if (server != null)
        server.stop();
      if (tempRoot != null)
        FileUtils.deleteQuietly(tempRoot);
    }

  };

  public void execute() throws MojoExecutionException, MojoFailureException {
    try {
      // Populate Circumflex with application configuration
      Circumflex.reinitWith(buildApplicationClassLoader());
      // Prepare application configuration
      Circumflex.update("application.classpath", getApplicationClasspathString());
      Circumflex.update("cx.router", DefaultScalaConsoleRouter.class);
      Circumflex.update("cx.port", port);
      // Grab scalaconsole JAR and extract it to temporary location
      if (tempRoot.isDirectory())
        throw new MojoExecutionException("Temp directory exists at " +
            tempRoot.getCanonicalPath() + ". Make sure temp directory does not exist.");
      FileUtils.forceMkdir(tempRoot);
      extractJar(getScalaConsoleJar(), tempRoot);
      // The `scalaconsole-webapp` folder should exist in extract location;
      // it is the web app root of our application.
      File webappRoot = new File(tempRoot, "scalaconsole-webapp");
      if (!webappRoot.isDirectory())
        throw new MojoExecutionException(
            "scalaconsole-webapp directory not found." +
                "circumflex-scalaconsole JAR must be broken.");
      Circumflex.update("cx.webappRoot", webappRoot.getCanonicalPath());
      // Make plugin exit nicely
      Runtime.getRuntime().addShutdownHook(shutdownHook);
      // Inject application classpath into current plugin class loading
      prepareClassLoader();
      // Prepare standalone Jetty
      server = new StandaloneServer();
      server.start();
      // Navigate to the default browser, if possible
      if (Desktop.isDesktopSupported()) {
        Desktop.getDesktop().browse(new URI("http://127.0.0.1:" + port + "/"));
      }
      // Now join the server process
      server.server().join();
    } catch (Exception e) {
      getLog().error("Could not start Circumflex Scala Console.", e);
    }
  }

  protected JarFile getScalaConsoleJar() throws Exception {
    URL jarUrl = null;
    for (Object o : pluginArtifacts) {
      Artifact artifact = (Artifact)o;
      File f = artifact.getFile();
      String name = f.getName();
      if (name.startsWith("circumflex-scalaconsole-") &&
          name.endsWith(".jar") &&
          f.isFile()) {
        jarUrl = f.toURI().toURL();
        break;
      }
    }
    if (jarUrl == null)
      throw new MojoExecutionException(
          "Could not locate circumflex-scalaconsole JAR in classpath.");
    return new JarFile(jarUrl.getFile());
  }

  protected void extractJar(JarFile jar, File root) throws Exception {
    Enumeration e = jar.entries();
    while (e.hasMoreElements()) {
      JarEntry entry = (JarEntry)e.nextElement();
      File file = new File(root, entry.getName());
      if (entry.isDirectory()) {
        FileUtils.forceMkdir(file);
      } else {
        InputStream is = jar.getInputStream(entry);
        FileOutputStream os = new FileOutputStream(file);
        try {
          IOUtils.copy(is, os);
        } finally {
          is.close();
          os.close();
        }
      }
    }
  }

}
