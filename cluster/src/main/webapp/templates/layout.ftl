[#ftl]

[#assign main]
<a id="header"
   class="heavy"
   href="/">
  <div class="cell logo no-mobile">
    <img src="/img/cx96.png"
         width="96"
         height="96"/>
  </div>
  <div class="cell">
    <div class="title">${msg['title']}</div>
    <div class="subtitle">
      <span>${conf.clusters?size} ${msg['clusters.configured']}</span>
    </div>
  </div>
</a>
${main}
<div id="footer">
  <div class="centered">
    <span>2008-2013</span>
    <span>&copy;</span>
    <a href="http://github.com/inca/circumflex"
       target="_blank">
      Circumflex Application Framework
    </a>
  </div>
</div>
[/#assign]

[#include "wrap.ftl"/]