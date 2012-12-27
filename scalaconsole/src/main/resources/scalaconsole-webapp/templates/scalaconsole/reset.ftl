[#ftl]

[#assign content]
<form action="/reset"
      method="post">
  <h1>Reset Scala Console?</h1>
  <p>Resetting Scala Console frees up any resources associated with your current session.</p>
  <p>If you continue, all your local definitions (variables, types and imports) will be unavailable.</p>
  <div id="repl-block" class="field-area hidden">
    <textarea id="cmd-b"
              rows="8"
              name="cmd"></textarea>
  </div>
  <div class="submits">
    <button type="submit">Confirm reset</button>
    <a href="/">
      <span>Go back</span>
    </a>
  </div>
</form>
[/#assign]

[#include "layout.ftl"/]
