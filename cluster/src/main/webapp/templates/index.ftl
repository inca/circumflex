[#ftl]

[#assign main]
<div id="content">
  <div class="letterbox">
    <div class="wrap content-output content-box">
      [#if !auth.principalOption??]
        [@me][#include "/locale/welcome.me.ftl"/][/@me]
      [#else]
        <h1>${msg['welcome']} ${auth.principal.name}!</h1>
        [#if conf.clusters?size > 0]
          [#list conf.clusters as cluster]
            <a href="/cluster/${cluster.id}"
               class="pill primary">
              <span>${cluster.id}</span>
              <div class="ctls">
                <div class="btn-group">
                  <div class="btn blank">
                    <img class="glyph"
                         src="http://cdn.savant.pro/img/glyph/32/computer.png"/>
                    <strong>${cluster.servers.children?size}</strong>
                  </div>
                  <div class="btn blank">
                    <img class="glyph"
                         src="http://cdn.savant.pro/img/glyph/32/share.png"/>
                    <strong>${cluster.nodes?size}</strong>
                  </div>
                </div>
              </div>
            </a>
          [/#list]
        [#else]
          <p class="no-items">${msg['clusters.empty']}</p>
        [/#if]
      [/#if]
      <div class="margin-top centered">
        <a href="/logout"
           class="btn important inverse">
          <span>${msg['logout']}</span>
        </a>
      </div>
    </div>
  </div>
</div>
[/#assign]

[#include "layout.ftl"/]