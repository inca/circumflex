package ru.circumflex.maven;

import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.project.MavenProject;

import java.io.File;
import java.io.FileOutputStream;
import java.io.PrintWriter;
import java.util.Properties;

public abstract class AbstractConfigureMojo extends AbstractCircumflexMojo {

    /**
     * @parameter default-value="true"
     */
    protected boolean skipUnresolved;

    public abstract File targetFile();

    public void execute() throws MojoExecutionException {
        try {
            Properties props = new Properties();
            for (Object key : project.getProperties().keySet()) {
                String value = project.getProperties().get(key).toString().trim();
                if (!(skipUnresolved && value.startsWith("${") && value.endsWith("}")))
                    props.put(key, value);
                else getLog().warn("Property with key " + key + " is unresolved. To include it, set 'skipUnresolved' to false.");
            }
            getLog().info("Writing Circumflex configuration to " + targetFile());
            FileOutputStream out = new FileOutputStream(targetFile());
            try {
                props.store(out, project.getName() + " Project Properties");
            } finally {
                out.close();
            }
        } catch (Exception e) {
            throw new MojoExecutionException("Could not configure Circumflex.", e);
        }
    }

}
