[#ftl]

[#assign application = application!{}/]

<div class="grid">
  <div class="w50">
    <div class="fieldbox">
      <label for="n">${msg['app.name']}</label>
      <div class="field">
        <div class="input">
          <input id="n"
                 type="text"
                 class="focus"
                 value="${application.name!}"
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
                 value="${application.title!}"
                 name="t"/>
        </div>
      </div>
    </div>
  </div>
</div>