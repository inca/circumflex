[#ftl]
{.super-title.medium}
Welcome, ${auth.principal.name}!

[#list conf.clusters as cluster]
## ${cluster.id}

{.remark}
${cluster.project.baseDir}

  [#list cluster.servers.children as server]
  ### ${server.address}

    [#list server.children as node]
    <table class="bordered rows">
      <tbody>
        <tr>
          <th colspan="2">${node.name}</th>
        </tr>
        [#list node.properties?keys?sort as k]
          <tr>
            <td>
              ${k}
            </td>
            <td>
              <em>${node.properties[k]}</em>
            </td>
          </tr>
        [/#list]
      </tbody>
    </table>
    [/#list]
  [/#list]

[/#list]