[#ftl]

[#assign content]
<div class="letterbox">
  <div class="wrap pad content-output">
    <h2>${msg['mgmt']}</h2>
      <a href="/applications"
         class="pill primary"
         title="${msg['apps']}">
        <span>${msg['apps']}</span>
      </a>
  </div>
</div>
[/#assign]

[#include "layout.ftl"/]