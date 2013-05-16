[#ftl]

[#assign main]
  [#if status.currentJob??]
    [#assign lines = status.currentJob.output/]
  <h3>${status.currentJob}</h3>
  <div id="job-output" class="relative">
    [#include "job-progress.p.ftl"/]
  </div>
  [#else]
    [#include "status.p.ftl"/]
  [/#if]
[/#assign]

[#include "layout.ftl"/]