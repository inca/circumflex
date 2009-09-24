[#ftl]
<html>
  <head>
    <title>Preved!!1</title>
  </head>
  <body>
    <h1>YO DOGG!11 THIS IS MA REAL FREEMARKA TEMPLATE!!!!1one</h1>
    <p>We have following variables in context:</p>
    <dl>
      [#list ctx.keys() as k]
      <dt>${k}</dt><dd>${ctx[k]}</dd>
      [/#list]
    </dl>
    <p>${ctx.keys().toList().mkString('::')}</p>
  </body>
</html>