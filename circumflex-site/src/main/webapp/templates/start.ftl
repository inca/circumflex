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
<p>After the build has successfully finished, Circumflex with all it's dependencies will be available in
  your local Maven 2 repository.
</p>
[/@section]
[/@page]
