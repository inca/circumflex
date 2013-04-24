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
          href="/img/cx16.png"
          type="image/x-icon">
    <link rel="stylesheet"
          href="/css/main.css"
          type="text/css">
    <script type="text/javascript"
            src="/locale.js">
    </script>
    <script type="text/javascript"
            src="http://cdn.savant.pro/js/jquery.js">
    </script>
    <script type="text/javascript"
            src="http://cdn.savant.pro/js/highlight.js">
    </script>
    <script type="text/javascript"
            src="http://cdn.savant.pro/js/ea.ui.js">
    </script>
    <script type="text/javascript">
      $(function() {
        eaui.init();
      });
    </script>
    <title>${msg['title']}</title>
  </head>
  <body>
    <div id="notices">
    </div>
    <div id="outer">
    ${main}
    </div>
  </body>
</html>
