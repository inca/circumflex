[#ftl]
<!DOCTYPE html>
<html>
<head>
  <meta http-equiv="content-type" content="text/html;charset=utf-8">
  <title>${title}</title>
  <link rel="stylesheet" href=".docco/main.css"/>
</head>
<body>
<div id="docco-index">
  <h1>${title}</h1>
  <ul>
  [#list dirs as dir]
    <li>
      <p>${dir}</p>
      <ul>
        [#list index[dir] as file]
          <li><a href="${dir}/${file}.html">${file}</a></li>
        [/#list]
      </ul>
    </li>
  [/#list]
  </ul>
</div>
</body>
</html>