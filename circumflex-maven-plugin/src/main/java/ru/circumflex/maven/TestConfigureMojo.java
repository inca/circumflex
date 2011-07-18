package ru.circumflex
package maven;
import java.io.File;

/**
 * @goal test-cfg
 */
public class TestConfigureMojo extends AbstractConfigureMojo {

  /**
   * @parameter default-value="${project.build.testOutputDirectory}/cx.properties"
   */
  protected File targetFile;

  public File targetFile() {
    return targetFile;
  }
}