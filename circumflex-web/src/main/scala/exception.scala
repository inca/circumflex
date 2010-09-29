package ru.circumflex.web

import ru.circumflex.core._

/*!# Exceptions

The `ResponseSentException` are thrown by response helpers and routes upon successful matching
and are caught by `CircumflexFilter`. They indicate that the response has been processed
successfully (and, possibly, already flushed to the client) and that no additional actions
need to be taken.
 */

/**
 * Indicates that the response has been flushed to the client and that further processing
 * must be stopped.
 *
 * For more information refer to
 * <a href="http://circumflex.ru/api/2.0/circumflex-web/exception.scala">exception.scala</a>.
 */
class ResponseSentException extends CircumflexException("The response has been sent.")