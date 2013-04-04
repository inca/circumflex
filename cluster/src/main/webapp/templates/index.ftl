[#ftl]

[#assign main]
<div class="letterbox">
  <div class="wrap content-output content-box">
    [#if !auth.principalOption??]
    [@me][#include "welcome.me.ftl"/][/@me]
    [#else]
    [/#if]
  </div>
</div>
[/#assign]

[#include "layout.ftl"/]