[#ftl]
<td width="32">
[#if unchecked??]
  <img title="${msg['status.unchecked']}"
       src="http://cdn.savant.pro/img/icons/32/orange_button.png"/>
[#elseif pid??]
  <img title="${msg['status.running']} (${pid})"
       src="http://cdn.savant.pro/img/icons/32/green_button.png"/>
[#else]
  <img title="${msg['status.stopped']}"
       src="http://cdn.savant.pro/img/icons/32/red_button.png"/>
[/#if]
</td>
<td>
  <div class="kicker code">${node.name}:${node.remote.port?c}</div>
  <div class="subtle">
  <code>${node.shortUuid}</code>
  [#if node.isJarBuilt]
    <span>${msg['node.built']}</span>
    <em>${node.jarBuiltDate?datetime}</em>
  [#else]
    <span>${msg['node.notBuilt']}</span>
  [/#if]
  </div>
</td>
<td width="48"
    class="right-align">
  <div class="relative">
    <a href="javascript:;"
       class="btn primary"
       data-switch="#nodemenu-${node.shortUuid}">
      <img src="http://cdn.savant.pro/img/glyph/32/cog.png"
           class="glyph"/>
      <span class="caret"></span>
    </a>
    <div id="nodemenu-${node.shortUuid}"
         class="dropdown-menu primary right">
    [#if pid??]
      <form action="/cluster/${cluster.id}/node/${node.shortUuid}/~stop"
            method="post"
            class="submission partial">
        <a href="javascript:;"
           class="submit">
          <img class="glyph"
               src="http://cdn.savant.pro/img/glyph/32/stop.png"/>
          <span>${msg['node.stop']}</span>
        </a>
      </form>
    [#else]
      <form action="/cluster/${cluster.id}/node/${node.shortUuid}/~run"
            method="post"
            class="submission partial">
        <a href="javascript:;"
           class="submit">
          <img class="glyph"
               src="http://cdn.savant.pro/img/glyph/32/play.png"/>
          <span>${msg['node.run']}</span>
        </a>
      </form>
    [/#if]
    </div>
  </div>
</td>