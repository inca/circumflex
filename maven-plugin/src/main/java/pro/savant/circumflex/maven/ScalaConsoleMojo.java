package pro.savant.circumflex.maven;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;

import pro.savant.circumflex.core.Circumflex;
import pro.savant.circumflex.scalaconsole.DefaultScalaConsoleRouter;

/**
 * @goal console
 * @requiresDependencyResolution complie+runtime+test
 */
public class ScalaConsoleMojo extends AbstractCircumflexMojo {

  public void execute() throws MojoExecutionException, MojoFailureException {
    try {
      Circumflex.update("application.classpath", getApplicationClasspathString());
      Circumflex.update("cx.router", DefaultScalaConsoleRouter.class);
      Circumflex.update("cx.port", 10888l);
    } catch (Exception e) {
      getLog().error("Could not start Circumflex Scala Console.", e);
    }
  }
}
