[#ftl]
[#include "./layout.ftl"]
[@page title="Users Guide"]
[@section id="toc"
          theme="paper"
          title="Table Of Contents"]
<ul>
  <li>Circumflex Web framework
    <ul>
      <li><a href="#context">Circumflex Context</a></li>
      <li><a href="#routers">Request Routers</a></li>
    </ul>
  </li>
</ul>
[/@section]

[@section id="context"
          theme="paper"
          title="Circumflex Context"]
<p><code>CircumflexContext</code>
  is essentially a thread local map instantiated by <code>CircumflexFilter</code>
  that allows different parts of Circumflex application to access the state of currently processed request.</p>
<p>You can access it from any parts of your application code, provided that these parts are executed "inside"
  <code>CircumfexFilter</code>. All you need to do is to add import to <code>Circumflex</code> singleton:</p>
<pre>${'
import ru.circumflex.core.Circumflex._
'?html}</pre>
<p>After that you may access thread local <code>CircumflexContext</code>:</p>
<pre>${'
ctx += "group" -> myGroup
// you may specify Circumflex object explicitly:
Circumflex.ctx += "user" -> myUser
'?html}</pre>
<p>You may use <code>CircumflexContext</code> to:</p>
<ul>
  <li>access various parameters, captured by <a href="#routers">routes</a>:
    <ul>
      <li>capture parts of URI (<a href="/guide/hello/world">example</a>):
      <pre id="ex01">${'
get("/guide/hello/(.+)") = "Saying hello to " + ctx("uri$1").get
'?html}</pre>
      </li>
      <li>capture headers parts (<a href="/guide/hello">example</a>):
        <pre id="ex02">${'
get("/guide/hello", headers("Host" -> "(.*)", "User-Agent" -> "(.*)")) =
      "Host is " + ctx("Host$1").get + "<br/>" +
      "user agent is " + ctx("User-Agent$1").get
'?html}</pre>
      </li>
    </ul>
  </li>
  <li>set parameters (for example, to pass data model to the view):
    <pre>${'
ctx += "numbers" -> (0 to 10)
// invoke some view
'?html}</pre>
  </li>
  <li>set response content type:
    <pre>${'
ctx.contentType = "application/js"
'?html}</pre>
  </li>
  <li>set response headers:
    <pre>${'
ctx.stringHeaders += "X-Accel-Redirect" -> "/downloads/resource.zip"
ctx.dateHeaders += "Last-Modified" -> System.currentTimeMillis
'?html}</pre>
  </li>
  <li>access lower-level Servlet API objects like <code>HttpServletRequest</code>, <code>HttpServletResponse</code>,
    <code>HttpSession</code>, <code>ServletContext</code> and so on:
    <pre>${'
println(ctx.request)
println(ctx.response)
'?html}</pre>
  </li>
</ul>
[/@section]

[@section id="routers"
          theme="paper"
          title="Request Routers"]
<p>Request Routers are the units of work of Circumflex application.
   They should extend <code>RequestRouter</code> class, and each Circumflex application
   can have only one <em>Main Router</em> – a special Request Router that gets executed
   on every incoming HTTP request.</p>
<p>Request Routers contain series of <em>routes</em>.
   Routes are designed to match incoming request using following criteria:</p>
<ul>
  <li>request methods (GET, POST, PUT or DELETE);</li>
  <li>request URI;</li>
  <li>zero or more request headers.</li>
</ul>
<p>Each route also has an attached block (a.k.a. route handler), it <em>must</em> return
  <code>HttpResponse</code>, that is render some view using one of Circumflex helpers, render
   arbitrary string (strings are implicitly converted to <code>TextResponse</code>), send
   redirect, a file or error, rewrite URL and so on.</p>
<p>Request Router body is only executed until one of the routes match the request. If matching
   occurs, Request Router immediately aborts further execution; instead the handler of the matched
   route gets executed and the <code>HttpResponse</code> that it returns is sent to the client.</p>
<p>For example consider the following Main Router (class declaration and imports are omitted for
   clarity):</p>
<pre>${'
get("/(hi|hello)") = "Hello world!"
println("Hmm... seems like it isn\'t greeting.")
get("/") = "Welcome!"
'?html}</pre>
<p>Now if <code>GET /</code> received, following line will be printed to console:</p>
<pre>${'Hmm... seems like it isn\'t greeting.'?html}</pre>
<p>And if it was <code>GET /hi</code> or <code>GET /hello</code>, then no lines would be printed
   to console; instead "Hello world!" would be sent to the client.</p>
<p>Routes use <a href="http://www.regular-expressions.info" target="_blank">Regular Expressions</a> to
   perform matching. When the route matches the request, matched groups become available in
  <code>CircumflexContext</code> as parameters: <code>${"uri$<group_index>"?html}</code> for request URI
   groups and <code>${"<header_name>$<group_index>"?html}</code> for headers groups
   (see <a href="#ex01">examples above</a>).</p>

<h2></h2>
[/@section]
[/@page]

