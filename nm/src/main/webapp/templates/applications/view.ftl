[#ftl]

[#assign main]
<div class="right-float">
  <a href="/applications/${application.name}/~edit"
     title="${msg['app.edit']}"
     class="btn primary">
    <img class="glyph" src="http://cdn.savant.pro/img/glyph/32/pencil.png"/>
  </a>
  <a href="/applications/${application.name}/~delete"
     class="btn important inverse"
     title="${msg['app.delete']}">
    <img class="glyph" src="http://cdn.savant.pro/img/glyph/32-inv/delete.png"/>
  </a>
</div>
<h2>${application.title}</h2>
  [#if application.children?size > 0]
  [#else]
  <p class="no-items">${msg['nodes.empty']}</p>
  [/#if]
<div class="centered margin-top">
  <a href="/applications"
     class="btn alternate"
     title="${msg['apps.toMgmt']}">${msg['apps.toMgmt']}</a>
</div>
[/#assign]

[#include "layout.ftl"/]