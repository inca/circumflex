package ru.circumflex.orm

import ru.circumflex.core.Circumflex

/* ## Configuration */

/**
 * `ORM` singletons aggregates all ORM-related interfaces into a single
 * configuration object.
 */
object ORM {

  /**
   * Connection provider.
   * Can be overriden with `orm.connectionProvider` configuration parameter.
   */
  val connectionProvider: ConnectionProvider = Circumflex.cfg("orm.connectionProvider") match {
    case Some(p: ConnectionProvider) => p
    case Some(c: Class[ConnectionProvider]) => c.newInstance
    case Some(s: String) => Class
        .forName(s, true, Circumflex.classLoader)
        .newInstance
        .asInstanceOf[ConnectionProvider]
    case _ => DefaultConnectionProvider
  }

  /**
   * SQL dialect.
   * Can be overriden with `orm.dialect` configuration parameter.
   */
  val dialect: Dialect = Circumflex.cfg("orm.dialect") match {
    case Some(d: Dialect) => d
    case Some(c: Class[Dialect]) => c.newInstance
    case Some(s: String) => Class.forName(s, true, Circumflex.classLoader)
            .newInstance
            .asInstanceOf[Dialect]
    case _ => DefaultDialect
  }

  /**
   * SQL type converter.
   * Can be overriden with `orm.typeConverter` configuration parameter.
   */
  val typeConverter: TypeConverter = Circumflex.cfg("orm.typeConverter") match {
    case Some(tc: TypeConverter) => tc
    case Some(c: Class[TypeConverter]) => c.newInstance
    case Some(s: String) => Class.forName(s, true, Circumflex.classLoader)
        .newInstance
        .asInstanceOf[TypeConverter]
    case _ => DefaultTypeConverter
  }

  /**
   * The schema name which is used if not specified explicitly.
   * Can be overriden with `orm.defaultSchema` configuration parameter.
   */
  val defaultSchemaName = Circumflex.cfg("orm.defaultSchema") match {
    case Some(s: String) => s
    case _ => "public"
  }

  /**
   * Transaction manager.
   * Can be overriden with `orm.transactionManager` configuration parameter.
   */
  val transactionManager: TransactionManager = Circumflex.cfg("orm.transactionManager") match {
    case Some(tm: TransactionManager) => tm
    case Some(c: Class[TransactionManager]) => c.newInstance
    case Some(s: String) => Class.forName(s, true, Circumflex.classLoader)
        .newInstance
        .asInstanceOf[TransactionManager]
    case _ => DefaultTransactionManager
  }

  /**
   * Shortcut for retrieving current transaction via `transactionManager.getTransaction`.
   */
  def tx = transactionManager.getTransaction

}