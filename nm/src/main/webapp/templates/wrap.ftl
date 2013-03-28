[#ftl]
<!doctype html>
<html>
  <head>
    <meta charset="utf-8"/>
    <meta http-equiv="content-type" content="text/html; charset=UTF-8" />
    <link rel="stylesheet"
          type="text/css"
          media="screen"
          href="http://cdn.savant.pro/css/themes/cx.css"/>
    <link rel="shortcut icon"
          href="/img/favicon.png"
          type="image/x-icon">
    <script type="text/javascript"
            src="http://cdn.savant.pro/js/jquery.js">
    </script>
    <script type="text/javascript"
            src="http://cdn.savant.pro/js/ea.ui.js">
    </script>
    <script type="text/javascript">
      $(function() {
        eaui.init();
      });
    </script>
    <title>Scala Web Console</title>
  </head>
  <body>
    [#assign notices = flash['notices']![]]
    <div id="notices">
      [#list notices as n]
        <div class="notice ${n.kind}">
          ${n.msg}
        </div>
      [/#list]
    </div>
  ${body}
  </body>
</html>
