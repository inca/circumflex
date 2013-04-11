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
    * ${node}
    [/#list]

  [/#list]

[/#list]