package ru.circumflex.web

import ru.circumflex.core._

/*!# Exceptions

The `ResponseSentException` are thrown by response helpers and routes upon successful matching
and are caught by `CircumflexFilter`. They indicate that the response has been processed
successfully (and, possibly, already flushed to the client) and that no additional actions
need to be taken.
 */
class ResponseSentException extends CircumflexException("The response has been sent.")