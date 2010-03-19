package ru.circumflex.maven;

import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.project.MavenProject;

import java.io.File;
import java.io.PrintWriter;

public abstract class AbstractConfigureMojo extends AbstractCircumflexMojo {

    /**
     * @parameter default-value="true"
     */
    protected boolean skipUnresolved;

    public abstract File targetFile();

    public void execute() throws MojoExecutionException {
        try {
            PrintWriter fw = new PrintWriter(targetFile());
            getLog().info("Writing Circumflex configuration to " + targetFile());
            try {
                for (Object key : project.getProperties().keySet()) {
                    String value = project.getProperties().get(key).toString().trim();
                    if (!(skipUnresolved && value.startsWith("${") && value.endsWith("}")))
                        fw.println(key + "=" + value);
                    else getLog().warn("Property with key " + key + " is unresolved. To include it, set 'skipUnresolved' to false.");
                }
            } finally {
                fw.close();
            }
        } catch (Exception e) {
            throw new MojoExecutionException("Could not configure Circumflex.", e);
        }
    }

}