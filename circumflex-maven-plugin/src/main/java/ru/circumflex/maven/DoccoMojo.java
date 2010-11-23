package ru.circumflex.maven;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import ru.circumflex.docco.DoccoBatch;
import ru.circumflex.core.Circumflex;
import java.util.Properties;

/**
 * @goal docco
 */
public class DoccoMojo extends AbstractCircumflexMojo {

  /**
   * @parameter expression="${customResources}"
   */
  protected String[] customResources;

  public void execute() throws MojoExecutionException, MojoFailureException {
    prepareClassLoader();
    if (!project.isExecutionRoot()) return;
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
}
