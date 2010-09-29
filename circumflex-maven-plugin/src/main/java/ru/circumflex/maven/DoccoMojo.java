package ru.circumflex.maven;

import org.apache.maven.artifact.DependencyResolutionRequiredException;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import ru.circumflex.docco.DoccoBatch;

import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;

/**
 * @goal docco
 */
public class DoccoMojo extends AbstractCircumflexMojo {

    /**
     * @parameter expression="${title}"
     */
    protected String title;

    /**
     * @parameter expression="${basePath}" default-value="."
     */
    protected String basePath;

    /**
     * @parameter expression="${outputDirectory}" default-value="target/docco"
     */
    protected String outputDirectory;

    /**
     * @parameter expression="${pageTemplate}" default-value="/docco-batch-page.html.ftl"
     */
    protected String pageTemplate;

    /**
     * @parameter expression="${indexTemplate}" default-value="/docco-index.html.ftl"
     */
    protected String indexTemplate;

    /**
     * @parameter expression="${filenameRegex}" default-value=".*\\.scala$"
     */
    protected String filenameRegex;

    /**
     * @parameter expression="${customResources}"
     */
    protected String[] customResources;

    public void execute() throws MojoExecutionException, MojoFailureException {
      prepareClassLoader();
      if (!project.isExecutionRoot()) return;
        File base = new File(basePath);
        File outDir = new File(outputDirectory);
        if (base.isDirectory()) {
            DoccoBatch db = new DoccoBatch(base, outDir);
            if (title != null)
                db.setTitle(title);
            db.setPageTemplate(pageTemplate);
            db.setIndexTemplate(indexTemplate);
            db.setFilenameRegex(filenameRegex);
            if (customResources != null)
                for (String res : customResources)
                    db.addCustomResource(res);
            try {
                getLog().info("Generating docco in " + outDir.getCanonicalPath());
                db.generate();
            } catch (IOException e) {
                throw new MojoExecutionException("Failed to generate docco", e);
            }
        } else {
            throw new MojoExecutionException("Specified basePath does not exist.");
        }
    }
}
