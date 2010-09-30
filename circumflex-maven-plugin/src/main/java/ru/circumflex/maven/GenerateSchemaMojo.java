package ru.circumflex.maven;

import org.apache.commons.io.FilenameUtils;
import ru.circumflex.core.Msg;
import ru.circumflex.orm.DDLUnit;
import ru.circumflex.orm.SchemaObject;
import ru.circumflex.orm.DeploymentHelper;
import org.apache.maven.plugin.MojoExecutionException;
import java.io.File;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.List;

/**
 * @goal schema
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
    ClassLoader oldCld = Thread.currentThread().getContextClassLoader();
    try {
      URLClassLoader cld = prepareClassLoader();
      Thread.currentThread().setContextClassLoader(cld);
      processSchema();
      processDeployments();
    } catch (Exception e) {
      throw new MojoExecutionException("DDL export failed.", e);
    } finally {
      if (closeConnectionProvider) {
        ddl.close();
      }
      Thread.currentThread().setContextClassLoader(oldCld);
    }
  }

  private void processSchema() {
    if (packages != null)
      for (String pkg : packages)
        processPackage(pkg);
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

  private void processPackage(String pkg) {
    String pkgPath = pkg.replaceAll("\\.", "/");
    try {
      URL outputDir = new URL("file://" + project.getBuild().getOutputDirectory() + "/");
      URL pkgUrl = Thread.currentThread().getContextClassLoader().getResource(pkgPath);
      if (pkgUrl != null) {
        File classDir = new File(outputDir.getFile() + pkgPath);
        if (!classDir.exists()) return;
        for (File f : classDir.listFiles()) try {
          if (f.getName().endsWith(".class")) {
            String className = pkg + "." +
                f.getName().substring(0, f.getName().length() - ".class".length());
            // Let's ensure that anonymous objects are not processed separately.
            if (!className.matches("[^\\$]+(?:\\$$)?")) continue;
            Class c = Thread.currentThread()
                .getContextClassLoader()
                .loadClass(className);
            SchemaObject so = null;
            try {
              // Try to process it as a singleton.
              Field module = c.getField("MODULE$");
              if (isSchemaObjectType(module.getType()))
                so = (SchemaObject)module.get(null);
            } catch (NoSuchFieldException e) {
              // Try to instantiate it as a POJO.
              if (isSchemaObjectType(c))
                so = (SchemaObject)c.newInstance();
            }
            if (so != null) {   // Found appropriate object.
              ddl.addObject(so);
              getLog().debug("Found schema object: " + c.getName());
            }
          }
        } catch (Exception e) {
          getLog().error("Failed to process a file: " + f.getAbsolutePath());
        }
      } else getLog().warn("Omitting non-existent package " + pkgPath);
    } catch (Exception e) {
      getLog().warn("Package processing failed: " + pkgPath, e);
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
