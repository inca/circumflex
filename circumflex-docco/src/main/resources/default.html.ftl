[#ftl]
<!DOCTYPE html>
<html>
<head>
  <meta http-equiv="content-type" content="text/html;charset=utf-8">
  <link rel="stylesheet" href="http://jashkenas.github.com/docco/resources/docco.css"/>
  <title>${title}</title>
</head>
<body>
<div id='container'>
  <div id="background"></div>
  <table cellspacing="0" cellpadding="0">
    <thead>
    <tr>
      <th class=docs><h1>${title}</h1></th>
      <th class=code></th>
    </tr>
    </thead>
    <tbody>
    [#list sections as sec]
    <tr id='section-${sec_index}'>
      <td class=docs>
        <div class="octowrap">
          <a class="octothorpe" href="#section-${sec_index}">#</a>
        </div>
      ${sec.doc}
      </td>
      <td class=code>
        <div class='highlight'><pre>${sec.code?html}</pre></div>
      </td>
    </tr>
    [/#list]
  </table>
</div>
</body>
</html>