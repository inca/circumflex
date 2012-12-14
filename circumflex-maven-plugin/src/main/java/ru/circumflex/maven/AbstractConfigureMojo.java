package pro.savant.circumflex.maven;

import org.apache.maven.plugin.MojoExecutionException;
import java.io.File;
import java.io.FileOutputStream;
import java.util.Properties;

public abstract class AbstractConfigureMojo extends AbstractCircumflexMojo {

  public abstract File targetFile();

  public void execute() throws MojoExecutionException {
    try {
      Properties props = collectProps();
      props.setProperty("application.classpath", getApplicationClasspathString());
      getLog().info("Writing Circumflex configuration to " + targetFile());
      FileOutputStream out = new FileOutputStream(targetFile());
      try {
        props.store(out, project.getName() + " Project Properties");
      } finally {
        out.close();
      }
    } catch (Exception e) {
      throw new MojoExecutionException("Could not configure Circumflex.", e);
    }
  }

}
