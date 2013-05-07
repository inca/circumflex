[#ftl]

<table width="100%"
       class="rows">
[#list cluster.servers.children as server]
  <tr>
    <td colspan="3">
      <div class="relative">
        <h3>
          <a href="javascript:;"
             class="no-underline"
             data-switch="#server-${server.shortUuid}">
            <span>${server.address}</span>
            <span class="caret"></span>
          </a>
        </h3>
        <div id="server-${server.shortUuid}"
             class="dropdown-menu primary">
          <div class="title">${msg['menu']}</div>
          <form action="/cluster/${cluster.id}/server/${server.shortUuid}/~build-server"
                method="post"
                class="submission partial">
            <a href="javascript:;"
               class="submit">
              <img class="glyph"
                   src="http://cdn.savant.pro/img/glyph/32/check.png"/>
              <span>${msg['job.build-server']}</span>
            </a>
          </form>
        </div>
      </div>
    </td>
  </tr>
  [#list server.children as node]
    <tr>
      <td width="32">
        <img title="${msg['status.unchecked']}"
             src="http://cdn.savant.pro/img/icons/32/orange_button.png"/>
      </td>
      <td>
        <div class="kicker">${node.name}</div>
        <div class="subtle">
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
            <div class="title">${msg['menu']}</div>
          [#-- TODO --]
          </div>
        </div>
      </td>
    </tr>
  [/#list]
[/#list]
</table>

<form class="submission partial margin-top"
      action="/cluster/${cluster.id}/~build-cluster"
      method="post">
  <a href="javascript:;"
     class="pill primary submit">
    <img class="glyph"
         src="http://cdn.savant.pro/img/glyph/32/settings.png"/>
    <span>${msg['job.build-cluster']}</span>
  </a>
</form>
<form class="submission partial margin-top"
      action="/cluster/${cluster.id}/~module-mci"
      method="post">
  <a href="javascript:;"
     class="pill primary submit">
    <img class="glyph"
         src="http://cdn.savant.pro/img/glyph/32/check_partial.png"/>
    <span>${msg['job.module-mci']}</span>
  </a>
</form>
<form class="submission partial margin-top"
      action="/cluster/${cluster.id}/~project-mci"
      method="post">
  <a href="javascript:;"
     class="pill primary submit">
    <img class="glyph"
         src="http://cdn.savant.pro/img/glyph/32/check.png"/>
    <span>${msg['job.project-mci']}</span>
  </a>
</form>