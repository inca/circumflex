# Circumflex Change Log

Version 2.0 was a great reorganization, so it is now a baseline for change log.

## 2.0.2

## 2.0.1

* Some configuration options are now available for `ScalaObjectWrapper` of
  Circumflex FreeMarker Helper.

  See [package.scala](http://circumflex.ru/api/2.0.1/circumflex-ftl/package.scala).

* Fixed [issue 68](https://github.com/inca/circumflex/issues#issue/68).

* `unique` and `uniqueAll` validators now work with persisted records too
  (by performing simple comparison with their persisted counterparts).

* Added new implicit convertion from strings into parameterless instances of
  `SimpleExpression`.

* Added new implicit convertion from boolean fields to predicates.

