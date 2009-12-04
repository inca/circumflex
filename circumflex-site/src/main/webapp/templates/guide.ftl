[#ftl]
[#include "./layout.ftl"]
[@page title="Users Guide"]
[@section id="toc"
          theme="paper"
          title="Table Of Contents"]
<ul>
  <li><a href="#context">Circumflex Context</a></li>
  <li><a href="#routers">Request Routers</a></li>
</ul>
[/@section]

[@section id="context"
          theme="paper"
          title="Circumflex Context"]
<p><em>Circumflex Context</em> is essentially a thread local map instantiated by CircumflexFilter
  that allows different parts of Circumflex application to access the state of currently processed request.</p>
<p>You can access it from any parts of your application code, provided that these parts are executed "inside"
  CircumfexFilter:</p>
<pre>${r'
import ru.circumflex.core.Circumflex._
ctx        // resolves to CircumflexContext instance for current request
'?html}</pre>
<p>You may use CircumflexContext to:</p>
<ul>
  <li>access various parameters, captured by <a href="#routers">routes</a>:
    <ul>
      <li>capture parts of URI (<a href="/guide/hello/world">example</a>):
      <pre>${r'
get("/guide/hello/(.+)") = "Saying hello to " + ctx("uri$1").get
'?html}</pre>
      </li>
      <li>capture headers parts (<a href="/guide/hello">example</a>):
        <pre>${r'
get("/guide/hello", headers("Host" -> "(.*)", "User-Agent" -> "(.*)")) =
      "Host is " + ctx("Host$1").get + "<br/>" +
      "user agent is " + ctx("User-Agent$1").get
'?html}</pre>
      </li>
    </ul>
  </li>
  <li>set parameters (for example, to pass data model to the view):
    <pre>${r'
ctx += "numbers" -> (0 to 10)
// invoke some view
'?html}</pre>
  </li>
  <li>set response content type:
    <pre>${r'
ctx.contentType = "application/js"
'?html}</pre>
  </li>
  <li>set response headers:
    <pre>${r'
ctx.stringHeaders += "X-Accel-Redirect" -> "/downloads/resource.zip"
ctx.dateHeaders += "Last-Modified" -> System.currentTimeMillis
'?html}</pre>
  </li>
  <li>access lower-level Servlet API objects like <em>HttpServletRequest</em>, <em>HttpServletResponse</em>,
    <em>HttpSession</em>, <em>ServletContext</em> and so on:
    <pre>${r'
println(ctx.request)
println(ctx.response)
'?html}</pre>
  </li>
</ul>
[/@section]

[@section id="routers"
          theme="paper"
          title="Request Routers"]
<p>Request Routers</p>
[/@section]
[/@page]

