# Circumflex Change Log

Version 2.0 was a great reorganization, so it is now a baseline for change log.

## 2.0.1

* Jetty dependencies are now marked as `optional` in Circumflex Web Framework.
  If you intend to use our `MockServer`, make sure to include necessary Jetty
  artifacts (`jetty`, `jetty-servlet-tester`). Consult with `pom.xml` of
  `circumflex-web` module for details.

* Some configuration options are now available for `ScalaObjectWrapper` of
  Circumflex FreeMarker Helper.

  See [package.scala](http://circumflex.ru/api/2.0.1/circumflex-ftl/package.scala).

* Fixed [issue 68](https://github.com/inca/circumflex/issues#issue/68).

* `unique` and `uniqueAll` validators now work with persisted records too
  (by performing simple comparison with their persisted counterparts).

