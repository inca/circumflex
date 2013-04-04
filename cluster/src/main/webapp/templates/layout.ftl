[#ftl]

[#assign main]
<a id="header"
   class="heavy"
   href="/">
  <div class="cell logo">
    <img src="/img/cx96.png"
         width="96"
         height="96"/>
  </div>
  <div class="cell">
    <div class="title">Circumflex Cluster Manager</div>
    <div class="subtitle">
      <span>${conf.clusters?size} cluster(s) configured</span>
    </div>
  </div>
</a>
${main}
<div id="footer">
  <div class="centered">
    <span>2008-2013</span>
    <span>&copy;</span>
    <a href="http://circumflex.savant.pro"
       target="_blank">
      Circumflex Application Framework
    </a>
  </div>
</div>
[/#assign]

[#include "wrap.ftl"/]