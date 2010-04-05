package ru.circumflex.maven;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;

/**
 * @goal docco
 */
public class DoccoMojo extends AbstractCircumflexMojo {

    /**
     * @parameter expression="${basePath}" default-value=""
     */
    protected String basePath;

    /**
     * @parameter expression="${outputDirectory}" default-value="target/docco"
     */
    protected String outputDirectory;

    /**
     * @parameter expression="${pageTemplate}" default-value="/single-page.html.ftl"
     */
    protected String pageTemplate;

    /**
     * @parameter expression="${indexTemplate}" default-value="/index.html.ftl"
     */
    protected String indexTemplate;

    /**
     * @parameter expression="${customResources}"
     */
    protected String[] customResources;

    public void execute() throws MojoExecutionException, MojoFailureException {

    }
}