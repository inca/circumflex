[#ftl]
[#include "/layout.ftl"]
[@page]
[@section id="config" title="Chapter 2. Configuring Circumflex"]
<p><em>Circumflex Configuration</em> is a set of application-wide settings
  (called <em>configuration parameters</em>) that are used by various Circumflex
  components.</p>
<p>There are three ways of configuring Circumflex components:</p>
<ul>
  <li><p>specify parameters in <code>cx.properties</code> file (which must be in
    CLASSPATH):</p>
    <pre>${'cx.router = myapp.Main'?html}</pre>
  </li>
  <li>provide parameters in runtime by calling <code>Circumflex.cfg</code> helper:
    <pre>${'Circumflex.cfg("cx.router") = classOf[Main]'?html}</pre>
  </li>
  <li>specify parameters in project's <code>pom.xml</code> in
    <code>&lt;properties&gt;</code> section and then use the <code>cfg</code> goal of
    <code>maven-cx-plugin</code> to copy them to <code>cx.properties</code>:
    <pre>${'
<!-- pom.xml -->
<project . . .>
  . . .
  <properties>
    <cx.router>myapp.Main</cx.router>
  </properties>
  . . .
  <build>
    <plugin>
      <groupId>ru.circumflex</groupId>
      <artifactId>maven-cx-plugin</artifactId>
      <version>0.2</version>
      <executions>
        <execution>
          <id>configure</id>
          <phase>generate-resources</phase>
          <goals>
            <goal>cfg</goal>
          </goals>
        </execution>
      </executions>
    </plugin>
  </build>
</project>'?html}</pre>
  </li>
</ul>
<p>All configuration parameters accept <code>String</code> values, however, some of them
  may also take advantage of specific types. For example, parameters that expect classes
  (like <code>cx.router</code>) may accept both <code>String</code> and <code>Class</code>
  constants.</p>
[@uc/]
[/@section]
[/@page]
