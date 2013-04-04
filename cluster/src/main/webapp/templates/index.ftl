[#ftl]

[#assign main]
<div id="content">
  <div class="letterbox">
    <div class="wrap content-output content-box">
      [#if !auth.principalOption??]
    [@me][#include "/locale/welcome.me.ftl"/][/@me]
    [#else]
        [@me][#include "/locale/dashboard.me.ftl"/][/@me]
      [/#if]
    </div>
  </div>
</div>
[/#assign]

[#include "layout.ftl"/]