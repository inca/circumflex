[#ftl]
[#include "/ui.ftl"]
[#macro page title=""]
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
        "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html>
  <head>
    <meta http-equiv="content-type" content="text/html; charset=UTF-8" />
    <link rel="stylesheet" type="text/css" media="screen" href="/css/main.css"/>
    <title>Circumflex Reference Documentation [#if title != ""]:: ${title}[/#if]</title>
  </head>
  <body>
    <div id="header">
      <h1><a href="/" title="Home">Circumflex</a></h1>
    </div>
    <div id="outer">
      [@bar id="nav"]
      <ul>
        <li><a href="/">TOC</a></li>
      </ul>
      [/@bar]
      <div id="content">
        [#nested/]
      </div>
      <div id="footer">
        <span class="years">2008-${currentYear}</span>
        <a class="home" href="http://${host}">${host}</a>
      </div>
    </div>
  </body>
</html>
[/#macro]

