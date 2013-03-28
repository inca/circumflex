[#ftl]
[#assign body]
<div id="outer">
  <a id="header"
     class="modal"
     href="/">
    <div class="cell">
      <div class="title">${msg['title']}</div>
    </div>
  </a>
  <div id="content" class="modal">
    <div class="letterbox">
      <div class="wrap pad">
      ${content}
      </div>
    </div>
  </div>
  <div id="footer">
    <div class="powered-by">
      <span>Powered by</span>
      <a class="circumflex"
         href="http://circumflex.savant.pro">Circumflex Application Framework</a>
    </div>
    <div class="copyright">
      <span class="year">2012</span>
      <span class="copy">&copy;</span>
      <a class="savantpro"
         href="http://savant.pro">SAVANT.PRO</a>
    </div>
  </div>
</div>
[/#assign]

[#include "wrap.ftl"/]
