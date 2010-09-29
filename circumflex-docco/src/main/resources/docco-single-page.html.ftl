[#ftl]
<!DOCTYPE html>
<html>
<head>
  <meta http-equiv="content-type" content="text/html;charset=utf-8">
  <title>${title}</title>
  <link href="http://circumflex.ru/css/docco.css"
        rel="stylesheet"
        type="text/css"
        media="screen, projection"/>
  <script type="text/javascript" src="http://circumflex.ru/js/highlight.pack.js">
  </script>
</head>
<body>
<div id="docco-page">
  <table cellspacing="0" cellpadding="0">
    <thead>
    <tr>
      <th class="docs"><h1>${title}</h1></th>
      <th class="code"></th>
    </tr>
    </thead>
    <tbody>
    [#list sections as sec]
    <tr id="section-${sec_index}">
      <td class="docs">
        <div class="octowrap">
          <a class="octothorpe" href="#section-${sec_index}">#</a>
        </div>
      ${sec.doc}
      </td>
      <td class="code">
        <pre class="scala"><code>${sec.code?html}</code></pre>
      </td>
    </tr>
    [/#list]
  </table>
</div>
</body>
</html>