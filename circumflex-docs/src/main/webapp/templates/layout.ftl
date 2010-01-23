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
[@nav id="navTop"/]
  <div id="content">
    [#nested/]
  </div>
[@nav id="navBottom"/]
  <div id="footer">
    <span class="years">2008-${currentYear}</span>
    <a class="home" href="http://circumflex.ru">circumflex.ru</a>
  </div>
</div>
</body>
</html>
[/#macro]

[#macro nav id=""]
[@bar id=id class="nav"]
<ul>
  <li><a href="/">Table of Contents</a></li>
</ul>
[/@bar]
[/#macro]

[#macro uc]
<p class="uc">
  This section is under construction. Thank you for your patience!
</p>
[/#macro]