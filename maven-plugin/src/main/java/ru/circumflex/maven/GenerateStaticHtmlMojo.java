package pro.savant.circumflex.maven;

import org.apache.maven.plugin.MojoExecutionException;
import java.io.File;
import pro.savant.circumflex.freemarker.*;

/**
 * @goal static-html
 */
public class GenerateStaticHtmlMojo extends AbstractCircumflexMojo {

  /**
   * @parameter expression="${templatesRoot}"
   */
  private File templatesRoot;

  /**
   * @parameter expression="${sourcePath}"
   */
  private String sourcePath;

  /**
   * @parameter expression="${targetDir}"
   */
  private File targetDir;

  public void execute() throws MojoExecutionException {
    prepareClassLoader();
    new StaticHtmlGenerator(templatesRoot, sourcePath, targetDir).generate();
  }

}
