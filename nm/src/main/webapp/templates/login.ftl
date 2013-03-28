[#ftl]

[#assign content]
    <form action="/login"
          method="post"
          class="submission content-output">
      <h2>${msg['login.title']}</h2>
      <div class="fieldbox">
        <label for="l">${msg['auth.username']}</label>
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
        <label for="p">${msg['auth.password']}</label>
        <div class="field">
          <div class="input">
            <input id="p"
                   type="password"
                   name="p"/>
          </div>
        </div>
      </div>
      <div class="submits margin-top centered">
        <input type="submit"
               class="btn primary"
               value="${msg['auth.login']}"/>
        <span>${msg['or']}</span>
        <a href="/">${msg['cancel']}</a>
      </div>
    </form>
[/#assign]

[#include "auth-layout.ftl"/]