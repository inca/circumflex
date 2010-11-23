package ru.circumflex.maven;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import ru.circumflex.docco.DoccoBatch;

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
    DoccoBatch db = new DoccoBatch();
    if (customResources != null)
      for (String res : customResources)
        db.addCustomResource(res);
    getLog().info("Generating docco in " + db.outputDirectory());
    db.generate();
  }
}
