[#ftl]

[#assign content]
<h1>Welcome to Simple Circumflex Web Application!</h1>
<p>Please take a moment to observe your project layout.</p>
<ul>
  <li><strong>pom.xml</strong>
    &mdash; Maven2 Project Object Model, an XML file that contains
    information about the project and configuration details used by Maven to build the project.
    It contains sensible default values for most projects. Basically you configure your
    project's dependencies, build names, source and target directories and build plugins using
    this configuration file. For more information, proceed to
    <a href="http://maven.apache.org/guides/introduction/introduction-to-the-pom.html"
       target="_blank">
      Introduction to the POM</a>.
  </li>
  <li><strong>src/main</strong>
    &mdash; main source directory of your project:
    <ul>
      <li><strong>scala/main.scala</strong>
        &mdash; your application's entry point called <em>Request Router</em>, it handles all incoming
        HTTP requests, matching them against <em>routes</em>: the first route that matches request
        is executed;</li>
      <li><strong>webapp</strong>
        &mdash; your Web application context root:
        <ul>
          <li><strong>WEB-INF/web.xml</strong>
            &mdash; the Java EE <em>deployment descriptor</em>,
            an XML configuration file that specifies, how your application will be deployed into
            <em>Servlet Container</em>.
            By default it just maps all URLs to <em>Circumflex Filter</em>.</li>
          <li><strong>public</strong>
            &mdash; a directory for static resources, e.g. stylesheets,
            scripts, images, etc.; resources under this directory are served directly by the
            container and are not processed by <em>Circumflex Filter</em>.</li>
          <li><strong>templates</strong>
            &mdash; a default location for FreeMarker templates; they are resolved relatively to this
            location by <em>Circumflex FreeMarker helper</em>.</li>
        </ul>
      </li>
      <li><strong>resources/Messages.properties</strong>
        &mdash; various messages for your application can be defined there for
        <a href="http://java.sun.com/docs/books/tutorial/i18n/index.html"
           target="_blank">Internationalization</a> purposes;</li>
      <li><strong>resources/cx.properties</strong>
        &mdash; Circumflex configuration parameters are specified here, by default it only contains the
        <em>cx.router</em> parameter, which points to your application Request Router class;</li>
    </ul>
  </li>
  <li><strong>src/test/scala</strong> &mdash; your tests source directory. It contains a simple
    spec out-of-box. You are free to add more sophisticated tests for your application.</li>
</ul>
<p>Please visit <a href="http://circumflex.ru" target="_blank">Circumflex website</a> for
   further information.</p>
<p style="font-weight:bold">
  We hope you'll get a lot of fun developing with Circumflex! Good luck!</p>
[/#assign]

[#include "/layout.ftl"/]
