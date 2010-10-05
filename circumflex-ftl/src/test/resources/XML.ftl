[#ftl]
<ul>
[#list root.child[0]?children as c]
  <li id="${c.@id}">
    <p>${c?node_name}</p>
    <p>${c?node_type}</p>
    <p>${c}</p>
  </li>
[/#list]
</ul>