[#ftl]
<!doctype html>
<html>
  <head>
    <meta charset="utf-8"/>
    <meta http-equiv="content-type" content="text/html; charset=UTF-8" />
    <link rel="stylesheet"
          type="text/css"
          media="screen"
          href="/css/main.css"/>
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
    <div id="outer">
      <a id="header"
         href="/">
      </a>
      <div id="content">
      ${content}
      </div>
      <div id="footer">
        <div class="powered-by">
          <span>Powered by</span>
          <a class="circumflex"
             href="http://github.com/inca/circumflex">Circumflex Application Framework</a>
        </div>
        <div class="copyright">
          <span class="year">2013</span>
          <span class="copy">&copy;</span>
          <a href="http://github.com/inca">Boris Okunskiy</a>
        </div>
      </div>
    </div>
  </body>
</html>
