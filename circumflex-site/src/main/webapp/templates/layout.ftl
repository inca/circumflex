[#ftl]
[#macro page title=""]
<html>
  <head>
    <meta http-equiv="content-type" content="text/html; charset=UTF-8" />
    <link rel="stylesheet" type="text/css" media="screen" href="/static/css/main.css"/>
    <title>Circumflex
      [#if title != ""]:: ${title}
      [#else]– lightweight Scala-based web application framework
      [/#if]
    </title>
  </head>
  <body>
    <div id="header">
      <div id="headerContent" title="Circumflex – lightweight Scala-based web application framework">
      </div>
    </div>
    <div id="outer">
      [@bar id="nav"]
      <ul>
        <li>one</li>
        <li>two</li>
        <li>three</li>
        <li>four</li>
        <li>five</li>
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

