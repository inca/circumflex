[#ftl]
<ul>
[#list list as l]
  <li>${l}</li>
[/#list]
</ul>

[#list range as r]${r}[#if r_has_next], [/#if][/#list]