[#ftl]

[#assign main]
  [#if status.currentJob??]
    [#assign lines = status.currentJob.output/]
  <h3>${status.currentJob}</h3>
  <div id="job-output">
    [#include "job-progress.p.ftl"/]
  </div>
  [#else]
  <p class="no-items">${msg['job.empty']}</p>
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
  [/#if]
[/#assign]

[#include "layout.ftl"/]