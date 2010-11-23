package ru.circumflex.maven;

import org.apache.maven.artifact.DependencyResolutionRequiredException;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.project.MavenProject;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

/**
 * Author: incarnate
 */
public abstract class AbstractCircumflexMojo extends AbstractMojo {

  /**
   * @parameter expression="${project}"
   * @readonly
   */
  protected MavenProject project;

  /**
   * @parameter default-value="true"
   */
  protected boolean skipUnresolved;

  protected URLClassLoader prepareClassLoader() throws MojoExecutionException {
    try {
      List<URL> urls = new ArrayList<URL>();
      for (Object o : project.getRuntimeClasspathElements()) {
        File f = new File(o.toString());
        String path = f.getAbsolutePath().replace(File.separatorChar, '/');
        if (f.isDirectory()) path += "/";
        urls.add(new URL("file", "localhost", path));
      }
      return URLClassLoader.newInstance(urls.toArray(new URL[urls.size()]),
          Thread.currentThread().getContextClassLoader());
    } catch (Exception e) {
      throw new MojoExecutionException("Could not prepare class loader.", e);
    }
  }

  protected Properties collectProps() {
    Properties result = new Properties();
    copyProps(project.getProperties(), result);
    copyProps(System.getProperties(), result);
    return result;
  }

  private void copyProps(Properties src, Properties dst) {
    for (Object key : src.keySet()) {
      String value = src.get(key).toString().trim();
      if (skipUnresolved && value.matches(".*\\$\\{.*\\}.*")) {
        getLog().warn("Property with key " + key + " is unresolved. To include it, set 'skipUnresolved' to false.");
      } else dst.put(key, value);
    }
  }

}
