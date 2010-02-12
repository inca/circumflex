[#ftl]
[#include "/layout.ftl"]
[@page]
[@section id="start" title="Chapter 1. Getting Started"]
<h2 id="overview">Overview</h2>
<p>Circumflex Web Framework (a.k.a. <em>circumflex-core</em>) is a lightweight Front Controller
  pattern implementation for quick and robust Web application development.</p>
<p>It uses <em>routes concept</em> to match requests and provides Domain-Specific Language
  that makes the development process natural, intuitive and extremely efficient. And simple.</p>
<p>Here is Circumflex hello-world application:</p>
<pre id="hello.scala">${r'
package myapp

import ru.circumflex.core.RequestRouter

class Main extends RequestRouter {
  get("/") = "Hello world!"
}'?html}</pre>
<p>Circumflex Web Framework does not rely on specific view technology, however Circumflex
  comes shipped with a helper for <a href="http://freemarker.org" target="_blank">FreeMarker</a>,
  a powerful feature-rich generic template engine. Circumflex FreeMarker Helper (a.k.a.
  <em>circumflex-ftl</em>) allows developers to use Scala core types as template's data model to
  easily create full-fledged web applications.</p>
<p>This book will provide detailed overview of Circumflex concepts and features.</p>
<h2 id="why">Why Circumflex?</h2>
<ul>
  <li>Circumflex components require minimum initial configuration, while still allowing
      developers to easily override defaults if necessary.</li>
  <li>Circumflex is based on Scala. It has all the benefits of Scala. It runs on JVM. It is fast.
      It is concious.</li>
  <li>Circumflex allows developers to choose the tools and libraries that best suit their
      particular needs, therefore it does not contain a comprehensive features list for
      every task a developer</li>
  <li>Circumflex is designed to use the powers of
    <a href="http://maven.apache.org" target="_blank">Apache Maven 2</a>
      software management platform. Adding Circumflex components to your project is a matter of
      few more lines in your <code>pom.xml</code>.</li>
  <li>All Circumflex components are relied on Scala support for Domain-Specific Languages (DSLs).
      They make the development process intuitive and extremely productive.</li>
  <li>Circumflex is completely free, with BSD-style license.</li>
</ul>
<h2 id="src">Build Circumflex from sources</h2>
<p>You can obtain latest Circumflex sources at
  <a href='http://github.com/inca/circumflex' target='_blank'>GitHub</a>:
</p>
<pre>$ git clone git://github.com/inca/circumflex.git</pre>
<p>It is recommended that you have an up-to-date version of Circumflex sources
  (they could be obtained at <a href="http://github.com/inca/circumflex">GitHub</a>).
  Working with Circumflex sources will help you clarify different details, which are
  not fully covered in this documentation.</p>
<p>Circumflex, like all Scala applications, runs on Java VM. Make sure that latest
  <a href="http://java.sun.com/javase/downloads/index.jsp" target="_blank">Java 6 SDK</a>
   is installed on your system.</p>
<p>Circumflex uses <a href="http://maven.apache.org" target="_blank">Apache Maven 2</a>
   for build management. If you don't already have Maven 2,
  <a href="http://maven.apache.org/download.html#Installation" target="_blank">install it</a>. Note,
   that some operating systems (e.g. Mac OS X 10.5 and higher) are shipped with Maven 2 by
   default. On some systems it is also preferrable to install Maven 2 via package managers.
   For example, on Debian or Ubuntu systems you may install Maven 2 by executing the following line:</p>
<pre>$ sudo apt-get install maven2</pre>
<p>If you are unfamiliar with Maven, you should probably read the
  <a href="http://maven.apache.org/guides/getting-started/maven-in-five-minutes.html"
     target="_blank">
    Maven in 5 Minutes
  </a> article or
  <a href="http://maven.apache.org/guides/getting-started/index.html"
     target="_blank">Getting Started Guide</a>.</p>
<p>Once you are ready to build, execute the following in Circumflex root directory:</p>
<pre>$ mvn clean install</pre>
<p>After the build has successfully finished, Circumflex with all it's dependencies will be
   available in your local Maven 2 repository (it may take a while to download dependencies
   the first time).</p>
<p>Please feel free to fork The Circumflex Project on GitHub and share with us any
  bug fixes or improvements you have in mind (or any thoughts on making Circumflex
  better). We highly appreciate your interest.</p>
<h2 id="setup">Set up Circumflex project</h2>
<p>Circumflex leverages <a href="http://scala-lang.org" target="_blank">Scala programming language</a>
  and <a href="http://maven.apache.org" target="_blank">Apache Maven 2</a> software management
  platform.</p>
<p>If you want to use Circumflex components in existing Maven2 projects, simply add following
  lines to the <code>dependencies</code> section of your <code>pom.xml</code>:</p>
<pre>${'
<dependency>
  <groupId>ru.circumflex</groupId>
  <artifactId>circumflex-core</artifactId>
  <version>2.0</version>
</dependency>
'?html}</pre>
<p>Or you may create new project with <code>circumflex-archetype</code>. To do this, change to
  the directory where you store your projects and run:</p>
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
<h2 id="layout">Sample project layout</h2>
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
          <li><strong>public</strong>
            – a directory for static resources, e.g. stylesheets,
            scripts, images, etc.; resources under this directory are served directly by the
            container and are not processed by <em>Circumflex Filter</em>.</li>
          <li><strong>templates</strong>
            – a default location for FreeMarker templates; they are resolved relatively to this
            location by <em>Circumflex FreeMarker helper</em>.</li>
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
        <pre>${'
<category name="ru.circumflex.core">
  <priority value="debug"/>
</category>'?html}</pre>
        <p>will result in every incoming request being logged to your console.
           If you are not familiar with <em>Apache log4j</em>, take a moment to observe the
          <a href="http://logging.apache.org/log4j/1.2/manual.html" target="_blank">
            Short introduction to log4j</a> article.</p>
      </li>
    </ul>
  </li>
</ul>
<h2 id="imports">Imports</h2>
<p>Circumflex resides in a single package. You should import all
  it's contents if you plan to use it in your code:</p>
<pre>${'import ru.circumflex.core._'}</pre>
<p>In order to save us some typing time we omit import statements in source code
  fragments throughout this book, assuming you have these imports in place
  where necessary.</p>
[/@section]
[/@page]
