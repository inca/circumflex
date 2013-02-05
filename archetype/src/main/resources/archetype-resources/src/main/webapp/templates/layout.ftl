[#ftl]
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
        "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html>
  <head>
    <meta http-equiv="content-type" content="text/html; charset=UTF-8" />
    <link rel="stylesheet"
          type="text/css"
          media="screen"
          href="/css/main.css"/>
    <title>Simple Circumflex Application</title>
  </head>
  <body>
    <div id="header">
    </div>
    <div id="outer">
      <div id="content">
        ${content}
      </div>
      <div id="footer">
        <span class="copyright">2008-${currentDate?string("yyyy")}</span> ©
        <a class="home" href="http://${request.headers['Host']!"localhost"}">
          ${request.headers['Host']!"localhost"}
        </a>
      </div>
    </div>
  </body>
</html>
