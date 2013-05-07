[#ftl]
[#if pid??]
<img title="${msg['status.running']} (${pid})"
     src="http://cdn.savant.pro/img/icons/32/green_button.png"/>
[#else]
<img title="${msg['status.stopped']}"
     src="http://cdn.savant.pro/img/icons/32/red_button.png"/>
[/#if]