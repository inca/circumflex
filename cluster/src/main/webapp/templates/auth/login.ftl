[#ftl]

[#assign main]
<form action="/auth/login"
      method="post"
      class="submission partial">
  <div class="grid">
    <div class="w66">
      <div class="super-title medium">${msg['login.title']}</div>
      <div class="fieldbox">
        <label for="l">${msg['login.username']}</label>
        <div class="field">
          <div class="input">
            <input id="l"
                   type="text"
                   class="focus"
                   name="l"/>
          </div>
        </div>
      </div>
      <div class="fieldbox">
        <label for="p">${msg['login.password']}</label>
        <div class="field">
          <div class="input">
            <input id="p"
                   type="password"
                   name="p"/>
          </div>
        </div>
      </div>
      <div class="margin-top centered">
        <input id="r"
               type="checkbox"
               value="true"
               name="r"/>
        <label for="r">${msg['login.rememberMe']}</label>
      </div>
    </div>
    <div class="w33 centered no-mobile">
      <a href="http://circumflex.savant.pro"
         class="no-icon no-underline">
        <img src="/img/cxn128.png"/>
      </a>
      <p>Become more efficient with Circumflex</p>
      <a href="http://circumflex.savant.pro">
        Learn more
      </a>
    </div>
  </div>
  <hr/>
  <div class="submits margin-top centered">
    <input type="submit"
           class="btn primary inverse"
           value="${msg['login.submit']}"/>
    <span>${msg['or']}</span>
    <a href="/">${msg['cancel']}</a>
  </div>
</form>
[/#assign]

[#include "layout.ftl"/]