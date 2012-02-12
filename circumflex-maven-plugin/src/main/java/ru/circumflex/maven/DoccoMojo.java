package ru.circumflex.maven;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import ru.circumflex.docco.DoccoBatch;
import ru.circumflex.core.Circumflex;

import java.io.File;
import java.util.Properties;

/**
 * @goal docco
 */
public class DoccoMojo extends AbstractCircumflexMojo {

  /**
   * @parameter expression="${customResources}"
   */
  protected String[] customResources;

  /**
   * @parameter expression="${project.build.sourceDirectory}
   */
  protected File sourceDirectory;

  /**
   * @parameter expression="${project.build.directory}
   */
  protected File buildDirectory;

  public void execute() throws MojoExecutionException, MojoFailureException {
    Thread.currentThread().setContextClassLoader(prepareClassLoader());
    // Configure Circumflex
    Properties props = collectProps();
    for (Object k : props.keySet()) {
      String key = k.toString();
      Circumflex.update(key, props.getProperty(key));
    }
    // Execute docco
    DoccoBatch db = new DoccoBatch();
    if (customResources != null)
      for (String res : customResources)
        db.addCustomResource(res);
    getLog().info("Generating docco in " + db.outputPath());
    db.generate();
  }

  @Override
  protected Properties getDefaultProperties() {
    Properties props = new Properties();
    props.setProperty("docco.basePath", sourceDirectory.getParentFile().getAbsolutePath());
    props.setProperty("docco.outputPath", new File(buildDirectory, "docco").getAbsolutePath());
    props.setProperty("docco.filenameRegex", ".*\\.(java|scala)$");
    return props;
  }
}
