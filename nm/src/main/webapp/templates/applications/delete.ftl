[#ftl]

[#assign main]
<form action="/applications/${application.name}"
      method="post"
      class="submission">
  <input type="hidden" name="_method" value="delete"/>
  [#include "/locale/app.delete.ftl"/]
  <div class="submits margin-top centered">
    <input type="submit"
           class="btn important inverse"
           value="${msg['app.delete']}"/>
    <span>${msg['or']}</span>
    <a href="/applications/${application.name}">${msg['cancel']}</a>
  </div>
</form>
[/#assign]

[#include "layout.ftl"/]