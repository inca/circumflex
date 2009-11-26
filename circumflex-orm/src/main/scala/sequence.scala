package ru.circumflex.orm

/**
 * Base functionality for SQL sequences.
 */
class Sequence[R](val table: Table[R],
                  val column: Column[Long, R])
    extends SchemaObject with JDBCHelper {

  /**
   * Retrieves next sequence value by invoking backend NEXTVAL query.
   */
  def nextValue: Long = {
    val sql = dialect.selectSequenceNextVal(this)
    sqlLog.debug(sql)
    auto(connectionProvider.getConnection.prepareStatement(sql)) {
      st => auto(st.executeQuery)(rs => if (rs.next) {
        return rs.getLong(1)
      } else
        throw new ORMException("Sequence NEXTVAL did not return a result: " + this.sequenceName))
    }
  }

  /* DDL */

  def sqlCreate = dialect.createSequence(this)

  def sqlDrop = dialect.dropSequence(this)

  def sequenceName = dialect.sequenceName(this)

}