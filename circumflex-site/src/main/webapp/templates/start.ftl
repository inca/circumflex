[#ftl]
[#include "./layout.ftl"]
[@page title="Quick Start"]
[@section theme="paper"
          title="Build from sources"]
<p>You can obtain latest Circumflex sources at
  <a href='http://github.com/inca/circumflex' target='_blank'>GitHub</a>:
</p>
<pre>$ git clone git://github.com/inca/circumflex.git</pre>
<p>Circumflex uses <a href="http://maven.apache.org" target="_blank">Apache Maven 2</a>
   for build management. If you don't already have Maven 2,
  <a href="http://maven.apache.org/download.html#Installation" target="_blank">install it</a>.
   If you are unfamiliar with Maven, you should probably read the
  <a href="http://maven.apache.org/guides/getting-started/maven-in-five-minutes.html"
     target="_blank">
    Maven in 5 Minutes
  </a> article or
  <a href="http://maven.apache.org/guides/getting-started/index.html"
     target="_blank">Getting Started Guide</a>.
</p>
<p>Once you are ready to build, execute the following in Circumflex root directory:</p>
<pre>$ mvn clean install</pre>
<p>After the build has successfully finished, Circumflex with all it's dependencies will be
   available in your local Maven 2 repository (it may take a while to download dependencies
   the first time).
</p>
[/@section]

[@section theme="paper"
          title="Create new project"]
<p>As soon as Circumflex is built, you are ready to create your first project. Change to your
   projects directory and run:</p>
<pre>$ mvn archetype:generate</pre>
<p>Choose the <strong>circumflex-archetype</strong> from your local catalog:</p>
<pre>
  Choose archetype:
  1: local -> circumflex-archetype (Circumflex Application Archetype)
  2: internal -> . . .
  Choose a number:  (1/2/3/ . . .) 17: : 1
</pre>
<p>Provide basic information about your project:</p>
<pre>
  Define value for groupId: : com.myapp
  Define value for artifactId: : myapp
  Define value for version:  1.0-SNAPSHOT: : 1.0
  Define value for package:  com.myapp: :
</pre>
<p>After you confirm your choice, a simple Circumflex application will be created. To run it, go to
   your project root (it matches <em>artifactId</em> that you have specified above) and execute the
   following:</p>
<pre>$ mvn compile jetty:run</pre>
The following lines indicate that your application is ready to serve requests:
<pre>
  [INFO] Started Jetty Server
  [INFO] Starting scanner at interval of 5 seconds.</pre>
<p>Now you may visit your application at
  <a href="http://localhost:8180" target="_blank">http://localhost:8180</a>.</p>
[/@section]

[@section theme="paper"
          title="Project layout"]
<p>The Simple Circumflex Application created above has following structure:</p>
<ul>
  <li><strong>pom.xml</strong>
    – Maven2 Project Object Model, an XML file that contains
    information about the project and configuration details used by Maven to build the project.
    It contains sensible default values for most projects. Basically you configure your
    project's dependencies, build names, source and target directories and build plugins using
    this configuration file. For more information, proceed to
    <a href="http://maven.apache.org/guides/introduction/introduction-to-the-pom.html"
       target="_blank">
      Introduction to the POM</a>.
  </li>
  <li><strong>src/main</strong>
    – main source directory of your project:
    <ul>
      <li><strong>scala/main.scala</strong>
        – your application's entry point called <em>Request Router</em>, it handles all incoming
        HTTP requests, matching them against <em>routes</em>: the first route that matches request
        is executed;</li>
      <li><strong>webapp</strong>
        – your Web application context root:
        <ul>
          <li><strong>WEB-INF/web.xml</strong>
            – the Java EE <em>deployment descriptor</em>,
            an XML configuration file that specifies, how your application will be deployed into
            <em>Servlet Container</em>.
            By default it just maps all URLs to <em>Circumflex Filter</em>.</li>
          <li><strong>static</strong>
            – a directory for static resources, e.g. stylesheets,
            scripts, images, etc.; resources under that directory are served directly by the
            container and are not processed by <em>Circumflex Filter</em>.</li>
          <li><strong>templates</strong>
            – a default location for FreeMarker templates;
            they are resolved relatively to this location by <em>Circumflex FreeMarker helper</em>.</li>
        </ul>
      </li>
      <li><strong>resources/Messages.properties</strong>
        – various messages for your application can be defined there for
        <a href="http://java.sun.com/docs/books/tutorial/i18n/index.html"
           target="_blank">Internationalization</a> purposes;</li>
      <li><strong>resources/cx.properties</strong>
        – Circumflex configuration parameters are specified here, by default it only contains the
        <em>cx.router</em> parameter, which points to your application Request Router class;</li>
      <li><strong>resources/log4j.xml</strong>
        – logger configuration for your project, you may use it to obtain more verbose information
        about the application's runtime activity, for example the following changes:
        <pre>${r'
<category name="ru.circumflex.core">
  <priority value="debug"/>
</category>'?html}</pre> will result in every incoming request being logged to your console.
      </li>
    </ul>
  </li>
</ul>
[/@section]
[/@page]

