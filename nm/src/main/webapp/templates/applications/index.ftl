[#ftl]

[#assign main]
<div class="right-float">
  <a href="/applications/~new"
     class="btn primary inverse"
     title="${msg['apps.create']}">${msg['apps.create']}</a>
</div>
<h2>${msg['apps']}</h2>

  [#if applications?size > 0]
    [#list applications as a]
    <a href="/applications/${a.name}"
       class="pill primary"
       title="${a.title}">${a.title}</a>
    [/#list]
  [#else]
  <p class="no-items">${msg['apps.empty']}</p>
  [/#if]
[/#assign]

[#include "layout.ftl"/]