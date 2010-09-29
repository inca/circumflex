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

/**
 * Author: incarnate
 */
public abstract class AbstractCircumflexMojo extends AbstractMojo {

    /**
     * @parameter expression="${project}"
     * @readonly
     */
    protected MavenProject project;

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

}
