package ru.circumflex.maven;

import org.apache.maven.plugin.MojoExecutionException;
import java.io.File;
import ru.circumflex.freemarker.*;

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
    ClassLoader oldCld = Thread.currentThread().getContextClassLoader();
    try {
      ClassLoader cld = prepareClassLoader();
      Thread.currentThread().setContextClassLoader(cld);
      new StaticHtmlGenerator(templatesRoot, sourcePath, targetDir).generate();
    } finally {
      Thread.currentThread().setContextClassLoader(oldCld);
    }
  }

}
