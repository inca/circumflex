[#ftl]

[#assign main]
<form action="/applications/${application.name}"
      method="post"
      class="submission">
  <h2>${msg['app.edit.title']}</h2>
  [#include "edit-base.p.ftl"/]
  <div class="submits margin-top centered">
    <input type="submit"
           class="btn primary inverse"
           value="${msg['Save']}"/>
    <span>${msg['or']}</span>
    <a href="/applications/${application.name}">${msg['cancel']}</a>
  </div>
</form>
[/#assign]

[#include "layout.ftl"/]