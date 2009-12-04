[#ftl]
[#include "./layout.ftl"]
[@page]
[@section id="circumflex-core"
          theme="paper"
          title="Circumflex Web Framework"]
<p>Circumflex Web Framework (a.k.a. <em>circumflex-core</em>) is a lightweight Front Controller
  pattern implementation for quick and robust Web application development.</p>
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
<p>For example, the following template (<em>test.ftl</em>):</p>
<pre>${r"
[#ftl]
<ul>
  [#list myList as elem]
  <li>${elem}</li>
  [/#list]
</ul>"?html}</pre>
<p>and following code:</p>
<pre id="test.scala">${r'
package myapp

import ru.circumflex.core._
import ru.circumflex.freemarker._

class Main extends RequestRouter with FreemarkerHelper {
  get("/test") = {
    ctx += "myList" -> List("one", "two", "three")
    ftl("test.ftl")
  }
}'?html}</pre>
<p>will result in following markup:</p>
<pre>${r'
<ul>
    <li>one</li>
    <li>two</li>
    <li>three</li>
</ul>'?html}</pre>
<p>The power and flexibility of Scala combined with various Circumflex helpers makes the
  development of arbitrary complex applications almost that simple, too.</p>
[@follow/]
[/@section]
[/@page]
