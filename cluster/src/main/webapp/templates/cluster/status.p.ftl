[#ftl]

<div class="pill primary">
[#if cluster.classesTimestamp??]
  <span>${msg['cluster.built']}</span>
  <em>${cluster.classesTimestamp?datetime}</em>
[#else]
  <span>${msg['cluster.notBuilt']}</span>
[/#if]
  <div class="ctls">
    <form class="submission partial inline"
          action="/cluster/${cluster.id}/~module-mci"
          method="post">
      <a href="javascript:;"
         class="btn primary submit"
         title="${msg['job.module-mci']}">
        <img class="glyph"
             src="http://cdn.savant.pro/img/glyph/32/check_partial.png"/>
      </a>
    </form>
    <form class="submission partial inline"
          action="/cluster/${cluster.id}/~project-mci"
          method="post">
      <a href="javascript:;"
         class="btn primary submit"
         title="${msg['job.project-mci']}">
        <img class="glyph"
             src="http://cdn.savant.pro/img/glyph/32/check.png"/>
      </a>
    </form>
  </div>
</div>

<table width="100%"
       class="rows">
[#list cluster.servers as server]
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
          <form action="/cluster/${cluster.id}/server/${server.shortUuid}/~build"
                method="post"
                class="submission partial">
            <a href="javascript:;"
               class="submit">
              <img class="glyph"
                   src="http://cdn.savant.pro/img/glyph/32/cogs.png"/>
              <span>${msg['job.build-server']}</span>
            </a>
          </form>
          <form action="/cluster/${cluster.id}/server/${server.shortUuid}/~deploy-main"
                method="post"
                class="submission partial">
            <a href="javascript:;"
               class="submit">
              <img class="glyph"
                   src="http://cdn.savant.pro/img/glyph/32/check.png"/>
              <span>${msg['job.deploy-server-main']}</span>
            </a>
          </form>
          <form action="/cluster/${cluster.id}/server/${server.shortUuid}/~deploy-backup"
                method="post"
                class="submission partial">
            <a href="javascript:;"
               class="submit">
              <img class="glyph"
                   src="http://cdn.savant.pro/img/glyph/32/check_partial.png"/>
              <span>${msg['job.deploy-server-backup']}</span>
            </a>
          </form>
          <form action="/cluster/${cluster.id}/server/${server.shortUuid}/~restart"
                method="post"
                class="submission partial">
            <a href="javascript:;"
               class="submit">
              <img class="glyph"
                   src="http://cdn.savant.pro/img/glyph/32/repeat.png"/>
              <span>${msg['job.restart-server']}</span>
            </a>
          </form>
        </div>
      </div>
    </td>
  </tr>
  [#list server.nodes as node]
    <tr id="node-${node.shortUuid}"
        data-check-url="/cluster/${cluster.id}/node/${node.shortUuid}/~status">
      [#assign unchecked = "true"/]
    [#include "/node/status.p.ftl"/]
    </tr>
  [/#list]
[/#list]
</table>

<form class="submission partial margin-top"
      action="/cluster/${cluster.id}/~build"
      method="post">
  <a href="javascript:;"
     class="pill primary small submit">
    <img class="glyph"
         src="http://cdn.savant.pro/img/glyph/32/settings.png"/>
    <span>${msg['job.build-cluster']}</span>
  </a>
</form>

<form action="/cluster/${cluster.id}/~restart"
      method="post"
      class="submission partial margin-top">
  <a href="javascript:;"
     class="pill primary small submit">
    <img class="glyph"
         src="http://cdn.savant.pro/img/glyph/32/repeat.png"/>
    <span>${msg['job.restart-cluster']}</span>
  </a>
</form>

<script type="text/javascript">

  (function() {

    var placeholders = {};

    function doCheck() {
      $("[data-check-url]").each(function() {
        var e = $(this);
        var url = e.attr("data-check-url");
        var id = e.attr("id");
        if (!placeholders[id])
          placeholders[id] = e.html();
        var placeholder = placeholders[id];
        $.ajax({
          type: "GET",
          url: url,
          dataType: "html",
          success: function(data) {
            e.empty().append(data);
            eaui.init(e);
          },
          error: function() {
            e.empty().append(placeholder);
            eaui.init(e);
          }
        });
      });
    }

    $(function() {
      doCheck();
      setInterval(doCheck, 10000);
    });

  })();

</script>

