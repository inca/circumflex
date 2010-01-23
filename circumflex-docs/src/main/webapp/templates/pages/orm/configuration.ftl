[#ftl]
[#include "/layout.ftl"]
[@page]
[@section id="config" title="Chapter 2. Configuration"]
<p>Circumflex ORM takes advantage of
  <a href="/core/configuration">configuration parameters for Circumflex Components</a>.
  Let's take a closer look at Circumflex configuration objects.</p>
<h2 id="conn-prv">Connection provider</h2>
<p><em>Connection provider</em> is used to acquire JDBC connections throughout
  the application. By default, Circumflex ORM uses
  <code id="conn-prv-def">DefaultConnectionProvider</code>. It behaves as follows:
</p>
<ul>
  <li>if <code>orm.connection.datasource</code> is set, use it to acquire data source
    from JNDI;</li>
  <li>if <code>orm.connection.datasource</code> is missing, construct a connection
    pool using <a href="http://www.mchange.com/projects/c3p0">c3p0</a> and following
    configuration parameters:
    <ul>
      <li>orm.connection.driver;</li>
      <li>orm.connection.url;</li>
      <li>orm.connection.username;</li>
      <li>orm.connection.password;</li>
    </ul>
  </li>
  <li>set <code>auto-commit</code> for each connection to <code>false</code>;</li>
  <li>set transaction isolation level of each connection to the value of
    <code>orm.connection.isolation</code> parameter (or use <code>READ COMMITTED</code>
    isolation by default).</li>
</ul>
<p>If c3p0 data source is used, you can fine tune it's configuration with
  <code>c3p0.properties</code> file (see
  <a href="http://www.mchange.com/projects/c3p0/index.html#configuration_properties">
    c3p0 documentation</a> for more details).</p>
<p>Though <code>DefaultConnectionProvider</code> is an optimal choice for most
  applications, you can, however, provide your own connection provider by implementing the
  <code>ConnectionProvider</code> trait and setting the <code>orm.connectionProvider</code>
  configuration parameter.
</p>
<h2 id="tx-mgr">Transaction manager</h2>
<p><em>Transaction demarcation</em> refers to setting the transaction boundaries.</p>
<p>Datatabase transaction boundaries are always necessary. No communication with the
  database can occur outside of a database transaction (this seems to confuse many
  developers who are used to the auto-commit mode). Always use clear transaction
  boundaries, even for read-only operations. Depending on your isolation level and
  database capabilities this might not be required but there is no downside if you
  always demarcate transactions explicitly.</p>
<p>There are several popular transaction demarcation patterns for various application types,
  most of which operate with some sort of "context" or "scope", to which a single
  transaction corresponds. For example, in web applications a transaction may correspond
  to a single request.</p>
<p><em>Transaction manager</em> aims to help developers demarcate their transactions
  by providing "contextual" transaction. By default it uses <code>ThreadLocal</code>
  to bind contextual transactions (each thread has it's own transactions). You can
  provide your own transaction manager by implementing the <code>TransactionManager</code>
  trait and setting the <code>orm.transactionManager</code> configuration parameter.</p>
<h2 id="dialect">Dialect</h2>
<p><em>Dialect</em> renders actual SQL fragments.</p>
<p>Different RDBMS vendors tend to extend
  SQL standards and provide their own features. If you discover that some features of
  the defaut dialect are imcompatible with your database vendor, feel free to implement
  your own dialect. The <code>Dialect</code> trait contains dozens of methods, so
  implementing a custom dialect from scratch can be somewhat tricky, so, rather than
  writing the whole thing from scratch, discover, which methods fail and override them.</p>
<p>To use a custom dialect, set <code>orm.dialect</code> configuration property.</p>
<h2 id="type-conv">Type converter</h2>
<p><em>Type converters</em> are used to read values from JDBC <code>ResultSet</code>s
  and to write values to JDBC <code>PreparedStatement</code>s.</p>
<p>Most of standard types are converted by JDBC driver automatically. But many
  database vendors tend to introduce various conveniant data types (such as
  network addresses, URLs, arrays and so on). These types cannot be handled automatically
  by JDBC. If you plan to work with non-standard types, provide your own
  <code>TypeConverter</code> implementation and set the <code>orm.typeConverter</code>
  configuration parameter accordingly.</p>
<h2 id="conf-params">Configuration parameters index</h2>
<dl>
  <dt>orm.connectionProvider</dt>
  <dd>Specifies the <a href="#conn-prv">connection provider</a> impementation that will
    be used throughout the application. The value can be:
    <ul>
      <li><code>ConnectionProvider</code> instance;</li>
      <li><code>${'Class[_ <: ConnectionProvider]'?html}</code>;</li>
      <li>a <code>String</code> with fully-qualified class name of connection
        provider implementation.</li>
    </ul>
    If none of above specified, the
    <a href="#conn-prv-def"><code>DefaultConnectionProvider</code></a> is used.
  </dd>
  <dt>orm.connection.driver</dt>
  <dd>Specifies a JDBC driver class to use by
    <a href="#conn-prv-def"><code>DefaultConnectionProvider</code></a>. The value
    can be a <code>String</code> or <code>Class</code>.</dd>
  <dt>orm.connection.url</dt>
  <dd>Specifies an URL to connect to database
    (e.g. <code>jdbc:postgresql://localhost:5432/mydb</code>).
    Only <code>String</code> values are accepted.</dd>
  <dt>orm.connection.username</dt>
  <dd>Specifies a database user name to establish connection.
    Only <code>String</code> values are accepted.</dd>
  <dt>orm.connection.password</dt>
  <dd>Specifies a database user password to establish connection.
    Only <code>String</code> values are accepted.</dd>
  <dt>orm.connection.isolation</dt>
  <dd>Specifies the default transaction isolation level.
    One of the following values expected:
    <ul>
      <li>none</li>
      <li>read_uncommitted</li>
      <li>read_committed</li>
      <li>repeatable_read</li>
      <li>serializable</li>
    </ul>
    If none of above specified, the
    <a href="#conn-prv-def"><code>DefaultConnectionProvider</code></a>
    will use <code>READ COMMITTED</code> isolation level.</dd>
  <dt>orm.transactionManager</dt>
  <dd>Specifies the <a href="#tx-mgr">transaction manager</a> impementation that will
    be used throughout the application. The value can be:
    <ul>
      <li><code>TransactionManager</code> instance;</li>
      <li><code>${'Class[_ <: TransactionManager]'?html}</code>;</li>
      <li>a <code>String</code> with fully-qualified class name of transaction
        manager implementation.</li>
    </ul>
    If none of above specified, the <code>DefaultTransactionManager</code> is used.
  </dd>
  <dt>orm.dialect</dt>
  <dd>Specifies the <a href="#dialect">dialect</a> impementation that will
    be used throughout the application. The value can be:
    <ul>
      <li><code>Dialect</code> instance;</li>
      <li><code>${'Class[_ <: Dialect]'?html}</code>;</li>
      <li>a <code>String</code> with fully-qualified class name of dialect
        implementation.</li>
    </ul>
    If none of above specified, the <code>DefaultDialect</code> is used.
  </dd>
  <dt>orm.typeConverter</dt>
  <dd>Specifies the <a href="#type-conv">type converter</a> impementation that will
    be used throughout the application. The value can be:
    <ul>
      <li><code>TypeConverter</code> instance;</li>
      <li><code>${'Class[_ <: TypeConverter]'?html}</code>;</li>
      <li>a <code>String</code> with fully-qualified class name of type
        converter implementation.</li>
    </ul>
    If none of above specified, the <code>DefaultTypeConverter</code> is used.
  </dd>
</dl>
[/@section]
[/@page]
