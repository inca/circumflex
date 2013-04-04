[#ftl strip_whitespace="true"]
[#compress]
var msg = {
  [#list msg?keys as k]
  "${k?js_string}": "${msg[k]?js_string}"[#if k_has_next],[/#if]
  [/#list]
};
[/#compress]
