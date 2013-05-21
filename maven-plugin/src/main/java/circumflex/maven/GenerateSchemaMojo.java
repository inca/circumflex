package circumflex.maven;

import org.apache.commons.io.FilenameUtils;
import circumflex.core.Msg;
import circumflex.orm.DDLUnit;
import circumflex.orm.SchemaObject;
import circumflex.orm.DeploymentHelper;
import org.apache.maven.plugin.MojoExecutionException;
import java.io.File;
import java.lang.reflect.Modifier;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

/**
 * @goal schema
 * @requiresDependencyResolution complie+runtime
 */
public class GenerateSchemaMojo extends AbstractCircumflexMojo {

  /**
   * @parameter
   */
  private List<String> packages;

  /**
   * @parameter expression="${drop}" default-value="false"
   */
  private boolean drop;

  /**
   * @parameter
   */
  private List<String> deployments;

  /**
   * @parameter expression="${deploymentsSuffix}" default-value=".cxd.xml"
   */
  private String deploymentsSuffix;

  /**
   * @parmeter expression="${closeConnectionProvider}" default-value="true"
   */
  private boolean closeConnectionProvider = true;

  private DDLUnit ddl = new DDLUnit();

  public void execute() throws MojoExecutionException {
    try {
      prepareClassLoader();
      processSchema();
      processDeployments();
    } catch (Exception e) {
      throw new MojoExecutionException("DDL export failed.", e);
    } finally {
      if (closeConnectionProvider) {
        ddl.close();
      }
    }
  }

  private void processSchema() throws Exception {
    List<URL> urls = getApplicationClasspath();
    if (packages != null)
      for (String pkg : packages)
        ddl.addPackage(
            scala.collection.JavaConversions.asScalaIterable(urls), pkg);
    if (ddl.schemata().size() > 0) {
      if (drop) ddl._drop();
      ddl._create();
      for (Msg msg : ddl.msgsArray()) {
        if (msg.key().equals("orm.ddl.info"))
          getLog().info(msg.param("status").get().toString());
        else if (msg.key().equals("orm.ddl.error"))
          getLog().error(msg.param("status").get().toString());
        getLog().debug(msg.param("sql").get().toString());
      }
    } else {
      getLog().info("No schema objects found to export.");
    }
  }

  private boolean isSchemaObjectType(Class c) {
    return SchemaObject.class.isAssignableFrom(c)
        && !Modifier.isAbstract(c.getModifiers())
        && !Modifier.isInterface(c.getModifiers());
  }

  private void processDeployments() {
    if (deployments == null) deployments = new ArrayList<String>();
    deployments.add("default.cxd.xml");
    for (String pkg : packages)
      findDeployments(pkg.replaceAll("\\.", "/"));
    findDeployments("");
    for (String d : deployments)
      processDeployment(d);
  }

  private void findDeployments(String relPath) {
    try {
      String path = project.getBuild().getOutputDirectory() + "/" + relPath;
      File dir = new File(FilenameUtils.separatorsToSystem(path));
      for (File f : dir.listFiles())
        if (f.getName().endsWith(deploymentsSuffix)) {
          String d = relPath.equals("") ? f.getName() : relPath + "/" + f.getName();
          if (!deployments.contains(d)) deployments.add(d);
        }
    } catch (Exception e) {
      getLog().warn("Could not process deployments for package " + relPath + ".");
    }
  }

  private void processDeployment(String deployment) {
    String path = project.getBuild().getOutputDirectory() + "/" + deployment;
    File f = new File(FilenameUtils.separatorsToSystem(path));
    if (!f.isFile()) {
      getLog().warn("Omitting non-existent deployment " + deployment + ".");
      return;
    }
    try {
      new DeploymentHelper(f).loadData();
      getLog().info("Deployment " + deployment + " processed successfully.");
    } catch (Exception e) {
      getLog().error("Could not process deployment " + deployment + ".", e);
    }
  }

}
