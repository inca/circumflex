# Circumflex Change Log

## Version 1.1

### Circumflex Web Framework

* Changed `apply` and `get` methods in `HashModel` to follow Scala convention
  for data structures.
* Refactored matchers, `&` method is used to make up composite matchers.
* Removed prefix matching for subrouters.
* The following methods:

  * `error`
  * `redirect`
  * `rewrite`
  * `done`
  * `sendFile`
  * `xSendFile`

  now throw `RouteMatchedException` immediately.
* Added `xhr_?` method.
* Fixed bugs in `Messages`, added some tests.
* Added support for HTTP `PATCH` method.

### Circumflex ORM

* Added methods for simplified queries (`get(id: Long)`, `all(limit: Int, offset: Int)`).
* Added `limit` and `offset` methods to Criteria API via auxiliary predicate.
* Changed `apply` and `get` methods in `ValueHolder` to follow Scala convention
  for data structures.
* Added infix-style `AND` and `OR` composite predicates.
* Removed `Subselect` class, all subqueries-related stuff now accept `SQLQuery`.
* Improved contextual transaction demarcation pattern by implementing proper transaction
dispatching within `TransactionManager.executeInContext` method.
* Added convenient method shortcuts for joins. `JoinType` predefines are renamed.
* Added named parameters feature to `Query` to allow reusing same queries.
* Added setters to `ValueHolder`.

### Circumflex Markdown

* Added some SmartyPants extensions.
* Added id attribute support for headers.
* Span elements are now processed inside headers.
* Defined a syntax to process markdown inside inline HTML.
* Added macro support.

### Circumflex Freemarker

* Hashes are now wrapped with `TemplateHashModelEx` which provides useful built-ins.

### Circumflex Docco

* Scaladocs are now left in code sections.

## Version 1.0

### Circumflex Web Framework

* Introduced Sinatra-like named parameters and greatly simplified matching.

### Circumflex Freemarker Views

* Changed default resolution priority for templating model (now webapp first, classpath second)

### Circumflex ORM

* Revisited the concepts and ideology and rewritten 95% of code.

## Version 0.3

* Added Circumflex Docco module.
* Added Circumflex Markdown module.

### Circumflex Web Framework

* Added sendFile with Content-Disposition: attachment.
* Added redirect with optional flashes.
* Added xSendFile feature.

### Circumflex Freemarker Views

* Not depending on circumflex-orm.
* DefaultConfiguration now can be used outside webapps.
* Removed textilej support to make the library more lightweight.

## Version 0.2.1

### Circmflex ORM

* Added helpers for simple expressions and named parameters.

### Circumflex Freemarker Views

* Added FTL singleton.
