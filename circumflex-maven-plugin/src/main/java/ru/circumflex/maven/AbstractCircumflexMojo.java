package ru.circumflex.maven;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.metadata.ArtifactMetadataSource;
import org.apache.maven.artifact.repository.ArtifactRepository;
import org.apache.maven.artifact.resolver.ArtifactResolutionResult;
import org.apache.maven.artifact.resolver.ArtifactResolver;
import org.apache.maven.artifact.resolver.filter.ScopeArtifactFilter;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.project.MavenProject;

import java.io.File;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.Collections;
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
   * @parameter expression="${localRepository}"
   * @readonly
   */
  protected ArtifactRepository localRepository;

  /**
   * @component
   */
  protected ArtifactResolver artifactResolver;

  /**
   * @component
   */
  protected ArtifactMetadataSource artifactMetaDataSource;

  /**
   * @parameter default-value="true"
   */
  protected boolean skipUnresolved;

  protected ClassLoader prepareClassLoader() throws MojoExecutionException {
    try {
      List<URL> urls = new ArrayList<URL>();
      for (Object o : project.getRuntimeClasspathElements())
        urls.add(new File(o.toString()).toURI().toURL());
      for (Object o : project.getTestClasspathElements())
        urls.add(new File(o.toString()).toURI().toURL());
      ArtifactResolutionResult deps = artifactResolver.resolveTransitively(
          project.getDependencyArtifacts(),
          project.getArtifact(),
          localRepository,
          project.getRemoteArtifactRepositories(),
          artifactMetaDataSource,
          new ScopeArtifactFilter(Artifact.SCOPE_COMPILE));
      for (Object o : deps.getArtifacts())
        urls.add(((Artifact)o).getFile().toURI().toURL());
      return URLClassLoader.newInstance(urls.toArray(new URL[urls.size()]),
          Thread.currentThread().getContextClassLoader());
    } catch (Exception e) {
      throw new MojoExecutionException("Could not prepare class loader.", e);
    }
  }

  protected Properties collectProps() {
    Properties result = new Properties();
    copyProps(getDefaultProperties(), result);
    copyProps(project.getProperties(), result);
    copyProps(System.getProperties(), result);
    return result;
  }

    protected Properties getDefaultProperties()
    {
        return new Properties();
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
