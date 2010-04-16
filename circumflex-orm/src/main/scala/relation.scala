package ru.circumflex.orm

import org.slf4j.LoggerFactory
import ORM._

/* ## Relation */

/**
 * *Relation* is a cornerstone of relational model. In general, each instance of
 * pesistent class is stored as a single record in a relation corresponding to that
 * class. Since Circumflex ORM employs the Active Relation design approach to
 * persistence, each persistent class should subclass `Relation`.
 */
class Relation[R] {


  /* ### Column creation */
  def intColumn = new NotNullColumn(dialect.integerType)

}