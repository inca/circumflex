[#ftl]
[#include "/layout.ftl"]
[@page]
[@section id="data-model" title="Chapter 3. Data Definition"]
<p>The data model of an application is usually developed at early phases of
  the software development cycle and can be thought of as a conceptual model
  of a system, which describes various entities involved in that system
  and their relationships.</p>
<p>There are various approaches to data modeling. Data-centric applications
  are usually modeled in a bottom-up fashion: the database schema is designed
  in first place while the classes, metadata, logic and other pieces of
  application are developed later, using database schema as their base.</p>
<p>Circumflex ORM greatly simplifies this design approach: you design
  database model right in your Scala application using
  <a href="#rels">relations</a>, <a href="#rels">records</a>
  and <a href="#aux">auxiliary database objects</a>. They allow
  fine-grained control over resulting DDL (data definition language)
  statements that are used to create real database schema for your
  application. The syntax for object definition closely resembles SQL, so it
  could be read even by those unfamiliar with Scala. These objects
  are subsequently used in your application to deal with all data-related
  operations.</p>
<p>The process of creating such constructs is refered to as
  <em>data definition</em>.</p>
<p>It is a common practice to group such objects in a single Scala package
  (it is not required, but usually is conveniant, for example, for generating
  database schema):</p>
<pre>${'package ru.circumflex.sandbox.model'?html}</pre>
<p>Let's take a close look at the essential components of Circumflex ORM
  which take part in data definition.</p>
<h2 id="rels">Relations and records</h2>
<p>In relational theory, a <em>relation</em> is a data structure which
  consists of a heading and an unordered set of tuples (or <em>records</em>)
  which share the same data type.</p>
<p>Relations and records are the core concepts of Circumflex ORM.
  The usage scenario of the two is trivial: relations are used to store and
  retrieve records.</p>
<p>Two subclasses of <code>Relation</code> are particularly useful for data definition:
  <code>Table</code> and <code>View</code>. Their instances correspond
  to actual tables and views of the database.</p>
<p>Each relation operates with it's own <code>Record</code> subclass, which you
  should provide as it's type parameter (we call such construct a <em>record class</em>).
  The instances of record classes correspond to actual rows in the database relation
  (we refer to these instances as <code>records</code>).</p>
<p>Circumflex ORM employs some conventions to avoid boilerplate code
  and to introduce some sort of type safety:</p>
<ul>
  <li>relations should be the companion objects of their corresponding record
    classes (the constructs share the same name, but <code>class</code> keyword
    is used with records and <code>object</code> keyword is used with relations);</li>
  <li>both relations and records have a single type parameter, which should
    always point to the actual <code>Record</code> implementation used in your
    application;</li>
  <li>each record should be uniquely identified within the entire system by it's
    <em>primary key</em> value; the relation should provide a single-column
    primary key constraint (<code>LongIdPK</code> is a handy trait that adds
    <code>id BIGINT NOT NULL PRIMARY KEY</code> column and a <em>sequence</em>
    to generate it's values).</li>
</ul>
<p>The following code fragment summarizes these conventions:</p>
<pre>${'
class User extends Record[User]     // {1}

object User extends Table[User]     // {2}
        with LongIdPK[User]         // {3}'?html}</pre>
<p>In the example above line #1 shows the declaration of the record class <code>User</code>,
  it's instances will be retrieved from and stored to the table <code>User</code>
  shown on line #2. Trait <code>LongIdPK</code> (line #3) effectively defines relation's
  primary key.</p>
<h2 id="tabs">Table basics</h2>
<p>In a relational database, the raw data is stored in tables, this subsection explains
  how tables are created with Circumflex ORM and what features are available to control
  what data is stored in the tables.</p>
<p>The process of creating tables involves defining <em>columns</em>, <em>constraints</em>,
  <em>record validators</em> and other <em>auxiliary objects</em>.</p>
<p>Let's look at the sample table definition:</p>
<pre>${'
object User extends Table[User]           // {1}
        with LongIdPK[User] {

  val login = stringColumn("login", 32)   // {2}
      .notNull                            // {3}
      .unique                             // {4}
      .validateNotEmpty                         // {5}
      .validatePattern("^[a-zA-Z0-9_]{1,32}$")  // {6}

  val name = stringColumn("name", 100)    // {7}
      .notNull                            // {8}
      .validateNotEmpty                   // {9}
}'?html}</pre>
<p>Here we see the declaration of the <code>User</code> table (#1).</p>
<p>Two <em>string columns</em> are added to the table,
  <code>login</code> and <code>name</code>; the columns will have
  <code>VARCHAR(32)</code> (#2) and <code>VARCHAR(100)</code> (#7) SQL data types
  respectively.</p>
<p>A <code>NOT NULL</code> constraint is applied to these columns (#3, #8).</p>
<p>A <code>UNIQUE</code> constraint is applied to <code>login</code> column (#4).</p>
<p>There are also 3 validators defined for columns: the
  <code>NotEmptyValidator</code> validators (#5, #9) will reject empty string values if you try
  to insert or update a record; the <code>PatternValidator</code> (#6) will reject strings
  that do not match specified regular expression (in this case value should contain
  a combination of at least one and at most 32 characters, which are latin letters, numbers or
  underscores.</p>
<h3 id="cols">Columns definition</h3>
<p>A table in a relational database is much like a table on paper: it consists of rows and
  columns. Each row corresponds to a <em>record instance</em>, while the columns designate,
  what sort of data each record contain. The number of the columns is fixed, and each
  column has a name.</p>
<p>Each column has a data type. The data type constrains the set of possible values that can
  be assigned to a column and assigns semantics to the data stored in the column so that it can
  be used for computations. For instance, a column declared to be of a numerical type will not
  accept arbitrary text strings, and the data stored in such a column can be used
  for mathematical computations. By contrast, a column declared to be of a character string type
  will accept almost any kind of data but it does not lend itself to mathematical calculations,
  although other operations such as string concatenation are available.</p>
<p>In Circumflex ORM columns also provide a way to convert values from Scala types to the types
  of relational databases.</p>
<p>The columns are defined inside the relation's body by declaring an immutable variable
  (with <code>val</code> keyword), which should be initialized with one of the
  <em>column definition methods</em> listed below:</p>
<table id="cols-methods"
       class="common">
  <thead>
  <tr>
    <th>Method</th>
    <th>SQL type</th>
    <th>Scala type</th>
  </tr>
  </thead>
  <tbody>
  <tr>
    <td><code>intColumn(name)</code></td>
    <td><code>INTEGER</code> or <code>INT4</code></td>
    <td><code>Int</code></td>
  </tr>
  <tr>
    <td><code>longColumn(name)</code></td>
    <td><code>BIGINT</code> or <code>INT8</code></td>
    <td><code>Long</code></td>
  </tr>
  <tr>
    <td><code>numericColumn(name)</code></td>
    <td><code>NUMERIC</code> (the precision and scale are vendor-specific)</td>
    <td><code>Double</code></td>
  </tr>
  <tr>
    <td><code>numericColumn(name, precision, scale)</code></td>
    <td><code>NUMERIC(precision, scale)</code></td>
    <td><code>Double</code></td>
  </tr>
  <tr>
    <td><code>stringColumn(name)</code></td>
    <td><code>TEXT</code> (for PostgreSQL only)</td>
    <td><code>String</code></td>
  </tr>
  <tr>
    <td><code>stringColumn(name, length)</code></td>
    <td><code>VARCHAR(length)</code></td>
    <td><code>String</code></td>
  </tr>
  <tr>
    <td><code>stringColumn(name, sqlType)</code></td>
    <td>specified by <code>sqlType</code> for vendor-specific string data type</td>
    <td><code>String</code></td>
  </tr>
  <tr>
    <td><code>booleanColumn(name)</code></td>
    <td><code>BOOLEAN</code></td>
    <td><code>Boolean</code></td>
  </tr>
  <tr>
    <td><code>timestampColumn(name)</code></td>
    <td><code>TIMESTAMP</code></td>
    <td><code>java.lang.Date</code></td>
  </tr>
  <tr>
    <td><code>dateColumn(name)</code></td>
    <td><code>DATE</code></td>
    <td><code>java.lang.Date</code></td>
  </tr>
  <tr>
    <td><code>timeColumn(name)</code></td>
    <td><code>TIME</code></td>
    <td><code>java.lang.Date</code></td>
  </tr>
  <tr>
    <td><code>column[T](name, sqlType)</code></td>
    <td>specified by <code>sqlType</code> for vendor-specific data type</td>
    <td>specified by type parameter <code>T</code></td>
  </tr>
  <tr>
    <td><code>virtualColumn[T](name)</code></td>
    <td>not applicable</td>
    <td>specified by type parameter <code>T</code></td>
  </tr>
  </tbody>
</table>
<p><strong>Note:</strong> <code>virtualColumn</code> is applicable only to views, we will talk
  about them later.</p>
<p>As the table above shows, Circumflex ORM provides column definition methods for the most
  popular data types. If you need to create a column with custom data type, all you have to do
  is use the <code>column[T](name, sqlType)</code> method. Note that using custom SQL types
  may affect the portability of your application.</p>
<h3 id="constrs">Constraints definition</h3>
<p>Columns and data types are a way to limit the kind of data that can be stored in a table.
  For many applications, however, the constraint they provide is too coarse. For example,
  a column containing a product price should probably only accept positive values. But there
  is no standard data type that accepts only positive numbers. Another issue is that you might
  want to constrain column data with respect to other columns or rows. For example, in a table
  containing product information, there should be only one row for each product number.</p>
<p>To that end, SQL allows you to define constraints on columns and tables. Constraints give
  you as much control over the data in your tables as you wish. If a user attempts to store
  data in a column that would violate a constraint, an error is raised by the database.</p>
<p><strong>Note:</strong> constraints and validators are quite separate concepts: the former
  enforces data integrity on the database side and does not participate in Circumflex ORM
  logic, the latter is optionally processed inside Circumflex ORM and has nothing to do with
  the database layer.</p>
<h4 id="constrs-chk">Check constraints</h4>
<p>Circumflex ORM supports table-level <em>check constraints</em>. Check constraints are
  the most generic ones: they allow you to specify a boolean expression
  (also refered to as <em>predicate</em>) that will be evaluated by the database
  each time a record is inerted or updated.</p>
<p>For example, consider a <code>Product</code> table below; to enforce only positive values
  in <code>price</code> column (#1) you may define a check constraint (#2):</p>
<pre id="ex-product">${'
object Product extends Table[Product]
        with LongIdPK[Product] {
  . . .
  val price = numericColumn("price")  // {1}
             .notNull
  check("price > 0")                  // {2}
}'?html}</pre>
<p>You can use any predicate expression as long as it's features are supported by the
  database of your choice.</p>
<h4 id="constrs-not-null">Not-null constraints</h4>
<p>A <em>not-null constraint</em> simply specifies that a column must not assume the null
  value.</p>
<p>It is created by invoking the <code>notNull</code> method on a column (#1):</p>
<pre>${'
val name = stringColumn("name", 200)
           .notNull      // {1}'?html}</pre>
<p>The <code>notNull</code> method returns the column instance to allow method chaining.</p>
<h4 id="constrs-uniq">Unique constraints</h4>
<p><em>Unique constraints</em> ensure that the data contained in a column or a group of
  columns is unique with respect to all the rows in the table.</p>
<p>It can be specified for a single column by invoking the <code>unique</code> method
  on it (#1):</p>
<pre>${'
val login = stringColumn("login", 32)
            .notNull
            .unique      // {1}'?html}</pre>
<p>To specify a unique constraint on a group of columns, define a table-level unqiue
  constraint (#1):</p>
<pre>${'
object Person extends Table[Person]
        with LongIdPK[Person] {
  . . .
  val passportSerial = stringColumn("pass_serial", 4)
                       .notNull
  val passportNumber = stringColumn("pass_nmbr", 6)
                       .notNull
  unique(passportSerial, passportNumber)    // {1}
}'?html}</pre>
<h4 id="constrs-pk">Primary keys</h4>
<p><em>Primary key constraint</em> is used to uniquely identify a record in a table.</p>
<p>Technically, a primary key constraint is simply a combination of a unique constraint
  and a not-null constraint. However, primary keys have a special meaning in Circumflex ORM.
  They are used to:</p>
<ul>
  <li>select, update or delete individual records;</li>
  <li>create associations between relations;</li>
  <li>maintain the transaction-scoped cache.</li>
</ul>
<p>The above implies that every relation must define a primary key (#1):</p>
<pre>${'
val id = longColumn("id")
         .notNull

primaryKey("id")      // {1}'?html}</pre>
<p><strong>Note:</strong> multi-column primary key constaints (also known as
  <em>composite primary keys</em>) are <strong>not</strong> supported by Circumflex ORM
  directly. If you need a composite key to maintain referential integrity, you can define
  a multi-column unique constraint between not-null columns and subsequently use it as
  a foreign key endpoint instead.</p>
<p>Circumflex ORM supports auto-incremented long columns (#1):</p>
<pre>${'
val id = longColumn("id")
         .notNull
         .autoIncrement'?html}</pre>
<p>The values for auto-increment columns are generated each time a record is inserted
  or <code>generateFields</code> method is called on a record.</p>
<p>It is a best practice to introduce an auto-incremented <code>id</code> column into
  each relation and make it a primary key. The trait <code>LongIdPK</code> is
  a handy shortcut for the following:</p>
<pre>${'
val id = longColumn("id")
         .autoIncrement
         .notNull
val idSeq = id.sequence.get
primaryKey(id)'?html}</pre>
<p>By default, a database <em>sequence</em> is used for generating values. You can override
  this behavior by providing your own <code>Column</code> implementation.</p>
<h4 id="constrs-fk">Foreign keys</h4>
<p>A <em>foreign key constraint</em> specifies that the values in a column (or a group
  of columns) must match the values appearing in some row of another relation. We say this
  maintains the <em>referential integrity</em> between two relations.</p>
<p>Let's assume you have the <code>Product</code> table:</p>
<pre>${'
object Product extends Table[Product]
        with LongIdPK[Product] {
  . . .
}'?html}</pre>
<p>Let's also assume you have a table storing orders of those products. We want to ensure
  that the <code>Order</code> table only contains orders of products that actually exist.
  So we define a foreign key constraint in the <code>Order</code> table that references
  the <code>Product</code> table (#1):</p>
<pre>${'
object Order extends Table[Order]
        with LongIdPK[Order] {
  . . .
  val product = longColumn("product_id")
                .notNull
                .references(Product)          // {1}
}'?html}</pre>

<h2 id="names">About generated names</h2>
[/@section]
[/@page]
