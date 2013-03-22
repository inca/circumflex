[#ftl]

[#assign content]
<div class="section-tabs toggler">
  <a href="javascript:;"
     data-for="#repl-expression"
     data-set-focus="#cmd-e">
    <span>Expression</span>
  </a>
  <a href="javascript:;"
     data-for="#repl-block"
     data-set-focus="#cmd-b">
    <span>Block</span>
  </a>
</div>
<div id="repl-output"
     class="section-body">
  <div class="prompt">Welcome to Scala console! Type Scala code below to have it evaluated.</div>
</div>
<form id="repl-input"
      action="/"
      method="post"
      class="submission">
  <div id="repl-expression" class="field">
    <div class="input">
      <input id="cmd-e"
             type="text"
             name="cmd"
             class="focus"/>
    </div>
    <a href="javascript:;"
       class="icon submit"
       title="Evaluate">
      <img class="glyph"
           src="${"http://cdn.savant.pro/img/glyph/32/play.png"}"/>
    </a>
  </div>
  <div id="repl-block" class="field-area hidden">
    <textarea id="cmd-b"
              rows="8"
              name="cmd"></textarea>
  </div>
  <div class="submits">
    <button type="submit">Evaluate</button>
    <a href="/reset">
      <span>Reset</span>
    </a>
  </div>
</form>
<script type="text/javascript">
  eaui.addListener("#repl-input", function() {
    var exprInput = $("#cmd-e");
    var output = $("#repl-output");
    var form = $("#repl-input");
    var action = form.attr("action");
    var currentExpr = "";
    var currentIdx = -1;

    form.bind("submit.eaui", function(ev) {
      $(".prompt", output).remove();
      var ph = eaui.placeholder();
      output.append(ph);
      var params = $(":input:visible", form).serializeArray();
      params.push({
        name: "__",
        value: new Date().getTime().toString()
      });
      flushHistory();
      $.ajax({
        url: action,
        type: "post",
        data: params,
        dataType: "html",
        success: function(data) {
          ph.replaceWith(data);
        },
        error: eaui.processAjaxError
      });
      ev.preventDefault();
      return false;
    });

    function flushHistory() {
      if (window.localStorage) {
        var history = getHistory();
        history.unshift(exprInput.val());
        if (history.length > 100)
          history = history.slice(0,99);
        localStorage.setItem("scalaconsole.history", JSON.stringify(history));
      }
      // Clear the field and state
      exprInput.val("").focus();
      currentExpr = "";
      currentIdx = -1;
    }

    function getHistory() {
      var history = [];
      if (window.localStorage) {
        try {
          history = JSON.parse(localStorage.getItem("scalaconsole.history"));
          if (!history)
            history = [];
        } catch (e) {}
      }
      return history;
    }

    if (window.localStorage) {
      $("#cmd-e").bind("keydown.eaui", function(ev) {
        var input = $(this);
        if (ev.keyCode == 0x26) {  // Up button
          ev.preventDefault();
          var h1 = getHistory();
          if ((currentIdx + 1) < h1.length) {
            currentIdx += 1;
            input.val(h1[currentIdx]);
          }
          return false;
        } else if (ev.keyCode == 0x28) {  // Down button
          ev.preventDefault();
          var h2 = getHistory();
          if (currentIdx > 0) {
            currentIdx -= 1;
            input.val(h2[currentIdx]);
          } else if (currentIdx == 0) {
            currentIdx = -1;
            input.val(currentExpr);
          }
          return false;
        }
      });
      $("#cmd-e").bind("keyup.eaui", function(ev) {
        if (currentIdx == -1)
          currentExpr = $(this).val();
      });
    }
  });
</script>
[/#assign]

[#include "layout.ftl"/]
