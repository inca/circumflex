# Circumflex change log

## Version 1.1

### Circumflex Web Framework

* Changed `apply` and `get` methods in `HashModel` to follow Scala convention
  for data structures.
* Refactored matchers, `&` method is used to make up composite matchers.

### Circumflex ORM

* Added methods for simplified queries (`get(id: Long)`, `all(limit: Int, offset: Int)`).
* Added `limit` and `offset` methods to Criteria API.
* Changed `apply` and `get` methods in `ValueHolder` to follow Scala convention
  for data structures.
* Added infix-style `AND` and `OR` composite predicates.

### Circumflex Markdown

* Added some smarty-pants extensions and some ideas from
[PHP Markdown Extra](http://michelf.com/projects/php-markdown/extra).

## Version 1.0

### Circumflex Web Framework

* Introduced Sinatra-like named parameters and greatly simplified matching.

### Circumflex Freemarker Views

* Changed default resolution priority for templating model (now webapp first, classpath second)

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
