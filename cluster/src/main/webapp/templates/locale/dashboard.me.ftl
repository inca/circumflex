[#ftl]
{.super-title.medium}
Welcome, ${auth.principal.name}!

[#list conf.clusters as cluster]
## ${cluster.id}

{.remark}
${cluster.project.baseDir}

<table class="bordered rows" width="100%">
  <tbody>
    [#list cluster.servers.children as server]
      <tr>
        <th>${server.address}</th>
      </tr>
      [#list server.children as node]
        <tr>
          <td>${node.name}</td>
        </tr>
      [/#list]
    [/#list]
  </tbody>
</table>
[/#list]