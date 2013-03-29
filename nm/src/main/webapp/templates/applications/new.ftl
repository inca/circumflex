[#ftl]

[#assign main]
<form action="/applications"
      method="post"
      class="submission">
  <h2>${msg['apps.create.title']}</h2>
  <div class="grid">
    <div class="w50">
      <div class="fieldbox">
        <label for="n">${msg['app.name']}</label>
        <div class="field">
          <div class="input">
            <input id="n"
                   type="text"
                   name="n"/>
          </div>
        </div>
        <label for="n" class="descr">${msg['app.name.descr']}</label>
      </div>
    </div>
    <div class="w50">
      <div class="fieldbox">
        <label for="t">${msg['app.title']}</label>
        <div class="field">
          <div class="input">
            <input id="t"
                   type="text"
                   name="t"/>
          </div>
        </div>
      </div>
    </div>
  </div>
  <div class="submits margin-top centered">
    <input type="submit"
           class="btn primary inverse"
           value="${msg['apps.create']}"/>
    <span>${msg['or']}</span>
    <a href="/applications">${msg['cancel']}</a>
  </div>
</form>
[/#assign]

[#include "layout.ftl"/]