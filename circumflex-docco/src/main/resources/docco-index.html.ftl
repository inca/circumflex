[#ftl]
<!DOCTYPE html>
<html>
<head>
  <meta http-equiv="content-type" content="text/html;charset=utf-8">
  <title>${title}</title>
  <link rel="stylesheet" href=".docco/main.css"/>
</head>
<body>
<div id="container">
  <div id="background"></div>
  <table cellspacing="0" cellpadding="0">
    <thead>
    <tr>
      <th class="docs"><h1>${title}</h1></th>
    </tr>
    </thead>
    <tbody>
    <tr>
      <td class="docs">
        <ul>
        [#list index.keys as dir]
          <li>${dir}
            <ul>
              [#list index[dir] as file]
                <li><a href="${dir}/${file}.html">${file}</a></li>
              [/#list]
            </ul>
          </li>
        [/#list]
        </ul>
      </td>
    </tr>
  </table>
</div>
</body>
</html>