[#ftl]

[#assign main]
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