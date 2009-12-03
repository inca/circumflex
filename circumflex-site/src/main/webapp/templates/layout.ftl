[#ftl]
[#macro page title=""]
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
        "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html>
<head>
  <meta http-equiv="content-type" content="text/html; charset=UTF-8" />
  <link rel="stylesheet" type="text/css" media="screen" href="/static/css/main.css"/>
  <title>Circumflex
    [#if title != ""]:: ${title}
    [#else]– lightweight Scala-based Web application framework and ORM
    [/#if]
  </title>
</head>
<body>
<div id="header">
  <div id="headerContent"
       title="Circumflex – lightweight open-source Scala-based Web Application Framework and ORM">
  </div>
</div>
<div id="outer">
  [@bar id="nav"]
  <ul>
    <li><a href="/">About</a></li>
    <li><a href="/start">Quick Start Guide</a></li>
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

[#macro section theme title="" id="" style=""]
<div class="section"
     [#if id != ""]id="${id}"[/#if]>
  <div class="${theme}" style="${style}">
    <div class="hdr">${title}</div>
    <div class="body">
      [#nested/]
      <div class="clearer"></div>
    </div>
  </div>
</div>
[/#macro]

[#macro bar theme="green" id="" style=""]
<div [#if id != ""]id="${id}"[/#if]
     class="bar"
     style="${style}">
  <div class="${theme}">
    <div class="w">
      <div class="e">
        <div class="c">
          [#nested/]
        </div>
      </div>
    </div>
  </div>
</div>
[/#macro]

[#macro follow]
<div class="highlight">
  <p>If you feel interested, please follow:</p>
  <ul>
    <li><a href="/start">Quick Start Guide</a> to quickly begin developing with Circumflex;</li>
    <li>Circumflex on <a href='http://github.com/inca/circumflex' target='_blank'>GitHub</a>;</li>
    <li><a href="http://incarnate.ru" target='_blank'>Incarnate Homepage</a> for tutorials,
      news and other stuff.</li>
  </ul>
</div>
[/#macro]

