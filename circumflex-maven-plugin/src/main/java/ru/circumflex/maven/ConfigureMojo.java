package ru.circumflex.maven;

import java.io.File;

/**
 * @goal cfg
 * @requiresDependencyResolution complie+runtime
 */
public class ConfigureMojo extends AbstractConfigureMojo {

  /**
   * @parameter default-value="${project.build.outputDirectory}/cx.properties"
   */
  protected File targetFile;

  public File targetFile() {
    return targetFile;
  }
}
