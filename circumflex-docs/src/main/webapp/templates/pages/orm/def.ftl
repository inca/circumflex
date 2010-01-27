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
  are subsequently used in your applocation to deal with all data-related
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
<p>In Circumflex ORM a <em>relation</em> is an object, whose type is derived
  from the <code>Relation</code> class, while a <em>record</em> is an instance
  of a specific <code>Record</code> subclass.</p>
<p>The usage scenario of the two is trivial: relations are used to store and
  retrieve records. Two subclasses of <code>Relation</code> are particularly
  useful for data definition: <code>Table</code> and <code>View</code>; their
  instances correspond to tables and views of database; they are sometimes
  refered to as <em>physical</em>, because they correspond to actual data
  structures in storage.</p>
<p>Circumflex ORM employs some conventions to avoid boilerplate code
  and introduce some sort of type safety:</p>
<ul>
  <li>both relations and records have a single type parameter, which should
    always point to the actual <code>Record</code> implementation used in your
    application;</li>
  <li>relations should be the companion objects of their corresponding records
    (the constructs share the same name, but <code>class</code> keyword is used
    with records and <code>object</code> keyword is used with relations);</li>
  <li>each record should be uniquely identified within the entire system by it's
    <em>primary key</em> value; the relation should provide a single-column
    primary key constraint (<code>LongIdPk</code> is a handy trait that adds
    <code>id BIGINT NOT NULL PRIMARY KEY</code> column and a <em>sequence</em>
    to generate it's values).</li>
</ul>
<p>The following code fragment summarizes these conventions:</p>
<pre>${'
class User extends Record[User]     // {1}

object User extends Table[User]     // {2}
        with LongIdPK[User]         // {3}'?html}</pre>
<p>In the example above line #1 shows the declaration of the <code>User</code> class,
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
<p>Note that <code>virtualColumn</code> is applicable only to views, we will talk
  about them later.</p>
[/@section]
[/@page]
