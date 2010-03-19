package ru.circumflex.maven;

import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.project.MavenProject;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.Properties;

/**
 * @goal cfg
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
