[#ftl]

[#macro bar theme="green" id="" style="" class=""]
<div [#if id != ""]id="${id}"[/#if]
     class="bar ${class}"
     style="${style}">
  <div class="${theme}">
    <div class="w">
      <div class="e">
        <div class="c">
          [#nested/]
        </div>
      </div>
    </div>
  </div>
</div>
[/#macro]

[#macro section theme="paper" title="" id="" style=""]
<div class="section ${theme}"
     [#if id != ""]id="${id}"[/#if]
     [#if style != ""]style="${style}"[/#if]>
  <h1>${title}</h1>
  <div class="body">
    [#nested/]
    <div class="clearer"></div>
  </div>
</div>
[/#macro]
