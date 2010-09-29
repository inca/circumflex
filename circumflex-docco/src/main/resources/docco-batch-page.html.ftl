[#ftl]
[#assign basePath][#list 1..depth as i]../[/#list][/#assign]
[#assign resPath]${basePath}.docco/[/#assign]
<!DOCTYPE html>
<html>
<head>
  <meta http-equiv="content-type" content="text/html;charset=utf-8">
  <title>${title}</title>
  <link rel="stylesheet" href="${resPath}main.css"/>
  <script type="text/javascript" src="${resPath}highlight.js">
  </script>
  <script type="text/javascript">
    hljs.initHighlightingOnLoad();
  </script>
</head>
<body>
<div id="container">
  <table cellspacing="0" cellpadding="0">
    <thead>
    <tr>
      <th class="docs">
        <h1><a href="${basePath}index.html" title="Back to index">&larr;</a>&nbsp;&nbsp;${title}</h1>
      </th>
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
        <pre><code>${sec.code?html}</code></pre>
      </td>
    </tr>
    [/#list]
  </table>
</div>
</body>
</html>