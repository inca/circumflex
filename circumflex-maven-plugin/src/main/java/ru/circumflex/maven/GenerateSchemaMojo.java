package ru.circumflex.maven;

import ru.circumflex.orm.DDLUnit;
import ru.circumflex.orm.SchemaObject;
import org.apache.maven.plugin.MojoExecutionException;
import java.io.File;
import java.lang.reflect.Modifier;
import java.net.URL;
import java.net.URLClassLoader;
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

    private DDLUnit ddl = new DDLUnit();

    public void execute() throws MojoExecutionException {
        ClassLoader oldCld = Thread.currentThread().getContextClassLoader();
        try {
            URLClassLoader cld = prepareClassLoader();
            Thread.currentThread().setContextClassLoader(cld);
            processSchema();
        } catch (Exception e) {
            throw new MojoExecutionException("DDL export failed.", e);
        } finally {
            Thread.currentThread().setContextClassLoader(oldCld);
        }
    }

    private void processSchema() {
        for (String pkg : packages)
            processPackage(pkg);
        if (ddl.schemata().size() > 0) {
            if (drop) ddl.drop();
            ddl.create();
            for (DDLUnit.Msg msg : ddl.msgsArray()) {
                if (msg instanceof DDLUnit.InfoMsg)
                    getLog().info(msg.body());
                else if (msg instanceof DDLUnit.ErrorMsg)
                    getLog().error(msg.body());
                getLog().debug(msg.sql());
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
                        // Let's ensure that anonymous objects are not processed separately
                        if (!className.matches("[^\\$]+(?:\\$$)?")) continue;
                        Class c = Thread.currentThread()
                                .getContextClassLoader()
                                .loadClass(className);
                        if (SchemaObject.class.isAssignableFrom(c)
                                && !Modifier.isAbstract(c.getModifiers())
                                && !Modifier.isInterface(c.getModifiers())) {
                            SchemaObject so = (SchemaObject)c.newInstance();
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
}