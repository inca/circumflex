# Circumflex Change Log

## 2.0.3

* Moved to Jetty 7.2.4 to allow Web Sockets support.

* Switched to Apache Http Client for testing (instead of Jetty's Servlet Tester).

* All ScalaDocs are stripped.

* Public directories are not delegated to underlying default servlets which
  tend to allow directory listings (security-related issue).

* URL-encoded JSESSIONID is not reported by `request.uri` anymore, thus it does not
  participate in regular matching.

* Added fenced code blocks like on GitHub (due to Markeven's nature, empty lines are
  condensed anyway).

* Added block postprocessors to Markeven.

* `ValidationException` moved to `circumflex-core`.

* Changed `orm.ehcacheManager` configuration.

* Trailing slashes are ignored when matching against routes ending with `/*`.

## 2.0.2

* Extracted common container functionality from `ValueHolder` into `circumflex-core`
  module.

* More strict rules in `subroute`: 404 is sent if no nested routes match.

* Records are evicted from `contextCache` on updates (credits to Scott Maher).

* Removed redundant `MsgGroup` class (changes affected `ValidationException` and
  the code which depended on them).

* The `any` route now does not perform method matching at all instead of just accepting
  standard HTTP methods.

* Added `rewrite` route as a quick replacement for heavy `forward`.

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

## Older changes

