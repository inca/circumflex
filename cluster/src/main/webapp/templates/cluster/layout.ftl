[#ftl]

[#assign main]
<div class="letterbox">
  <div class="wrap">
    <div class="breadcrumbs primary border-bottom">
      ${breadcrumbs!}
      <a href="/cluster/${cluster.id}">
        ${cluster.id}
      </a>
      <a href="/"
         title="${msg['home']}">
        <img class="glyph always"
             src="http://cdn.savant.pro/img/glyph/32/home.png"/>
      </a>
    </div>
    <div class="content-box content-output rich-links">
    ${main}
    </div>
  </div>
</div>
[/#assign]

[#include "../layout.ftl"/]