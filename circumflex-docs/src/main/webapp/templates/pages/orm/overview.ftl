[#ftl]
[#include "/layout.ftl"]
[@page]
[@section id="overview" title="Chapter 1. Overview"]
<p>Relational databases are widely used by various kinds of applications
  for efficient data storage and retrieval purposes. It is a known fact, that
  Relational Models and Object-Oriented Paradigms employ different data
  structuring approaches. Though Java platform provides database connectivity
  API to access relational data, dealing with it directly is often inconvenient
  and generally results in poor maintainability, extensibility and portability,
  especially in large applications.
</p>
<p>The term Object/Relational Mapping (ORM) refers to the technique of mapping
  a data representation from an object model to a relational data model.
  ORM tools may significantly speed up development by eliminating boilerplates
  for common CRUD (create-read-update-delete) operations, make applications
  more portable by incapsulating vendor-specific SQL dialects, provide
  object-oriented API for querying, allow navigating associations between
  objects transparently and much more.
</p>
<h2 id="jpa-dis">Disadvantages of Java Persistence API</h2>
<p>Java Persistence API tend to fulfill the inconsistencies between the two
  paradigms by completely separating domain model from relational model
  (by using the middle-tier, the mapping metadata). Domain model is usually built with
  POJOs (Plain Old Java Objects), also known as <em>entities</em>, such objects
  know nothing about actual database schema and storage routines. The mapping metadata
  is constructed around persistent classes; JPA-based frameworks use it to do all
  the work.
</p>
<p>However, this approach has several disadvantages. Here are some of them:</p>
<ul>
  <li>
    <p>leaky abstractions:</p>
    <ul>
      <li>from time to time you'll find yourself having to jump out of the object
        world, trying to identify the particular query in relational world and
        then having to figure out a way to convince your persistence layer to
        do exactly this;
      </li>
      <li>if you run into problems, you'll have to understand all the nitty-gritty
        details (for huge frameworks, such as
        <a href="http://hibernate.org">Hibernate</a>, it could become a real
        nightmare);
      </li>
      <li>despite the fact that entities are supposed to know nothing about
        persistence layer, this is not quite true: entities and their state
        are implicitly tracked by JPA environment; such entities typically
        don't work outside of JPA session;
      </li>
    </ul>
  </li>
  <li>
    <p>less efficiency in data-centric applications:</p>
    <ul>
      <li>
        schema-generation tools in classic ORM frameworks usually provide poor
        support for various auxiliary database objects, such as triggers,
        stored procedures, etc.;
      </li>
      <li>
        working with auxiliary objects is usually non-trivial, it requires various
        workaround techniques and lead to poor code organization;
      </li>
      <li>
        JPA environment tries to keep entities and database records in synchronized
        state; sometimes it can interfere with backend logic;
      </li>
    </ul>
  </li>
  <li>it is a common practice to generate proxies instead of real objects
    to achieve several goals, such as lazy loading; there are many pitfalls
    regarding proxy objects;
  </li>
  <li>persistence layer overhead: you lose at least 5% of overall performance on
    reflection, proxies and other stuff; also such frameworks tend to have
    significant memory footprints;
  </li>
  <li>
    <p>limited Scala support:</p>
    <ul>
      <li>several mapping techniques are not applicable to Scala applications
        (nested annotations are not supported in Scala 2.7);</li>
      <li>several features, such as validation, requre JavaBeans conventions;</li>
      <li>Scala collections are not supported out-of-box.</li>
    </ul>
  </li>
  <li>very steep learning curve: in order to build complex and efficient
    applications you'll need to know SQL in a profound way and be aware of all
    the subtles of your persistence framework.</li>
</ul>
<h2 id="cx-orm-goals">Circumflex ORM Goals</h2>
<p><a href="http://circumflex.ru/orm">Circumflex ORM</a> aims to eliminate
  <a href="#jpa-dis">JPA disadvantages</a> and to provide robust, concise,
  fast and elegant persistence framework for data-centric applications:</p>
<ul>
  <li>it does not separate relational model from object model: entities "know"
    where they come from (or where they should go);</li>
  <li>it takes an advantage of Active Record design pattern
    (as opposed to JPA's POJOs with mapping metadata);</li>
  <li>it <em>requires</em> knowledge of SQL and relational theory,
    but framework itself is quite simple: you define the schema of
    your data using object-oriented constructs and high-level abstractions
    (<em>tables</em>, <em>views</em>, <em>columns</em>, <em>indices</em>,
    <em>queries</em>, etc.) that very closely resemble the native SQL syntax;</li>
  <li>it simplifies traditional SQL stuff wherever possible, but does not limit
    query capabilities in any way: you can use object-oriented joins, unions
    (and other set operations), WHERE-conditions, grouping, ordering, aggregate
    functions, arbitrary projections, HAVING-predicates and so on;</li>
  <li>is supports associations prefetching (fetching whole object graphs in one
    query) and transaction-scoped caches;</li>
  <li>there are no persistence contexts, no sessions, no proxies and, therefore,
    no "gotchas": everything works precisely the way you expect it to work;</li>
  <li>custom dialects are fairly easy to implement;</li>
  <li>experimental <em>Data Internationalization</em> feature relies on
    <a href="http://postgresql.org">PostgreSQL</a> support for custom
    transaction-scoped variables and updatable views to allow storing and
    retrieving data in specific language; subsequent updates of localizable
    columns do not interfere with different locales.</li>
</ul>
<p>Circumflex ORM aims to be the optimal choice for data-centric applications
  written in Scala: it supports sophisticated schema, complex queries and
  queries in native SQL. If you develop such application, you should probably keep
  an eye on the documentation of the database you've chosen.</p>
<p>Circumflex ORM is also a nice choice for beginners and simple applications
  (despite the common opinion that such tool are an overkill in both metioned cases)
  &mdash; you will find it both easy-to-use and efficient even for small applications
  that only operate with a couple of tables, while it's similarity to SQL in data
  definition semantics and queries will help the beginners to better understand
  relational concepts.</p>
<h2 id="architecture">Architecture</h2>
<p>The following diagram depicts high-level view of Circumflex ORM architecture.</p>
<div class="media">
  <img src="/img/orm-arch.png"
       alt="Circumflex ORM Architecture"
       title="Circumflex ORM Architecture"/>
</div>
<p>Let's take a look at the configuration basics.</p>
<dl>
  <dt>ConnectionProvider</dt>
  <dd>opens JDBC connections.</dd>
  <dt>TransactionManager</dt>
  <dd>defines transaction demarcation strategy.</dd>
  <dt>Dialect</dt>
  <dd>renders SQL fragments.</dd>
  <dt>TypeConverter</dt>
  <dd>converts Scala objects to backend data chunks and vice versa.</dd>
</dl>
<p>That's fairly simple. Now let's look at the Schema/Model part.</p>
<dl>
  <dt>Relation</dt>
  <dd>corresponds to database table or view and provide CRUD operations to specific
    types of records.</dd>
  <dt>Record</dt>
  <dd>represents a persistent object that corresponds to a single row in database
    relation.</dd>
  <dt>Auxiliary schema object</dt>
  <dd>represents an abstract database object for schema generation purposes.</dd>
</dl>
<p>We will describe each concept in later chapters.</p>
<h3 id="imports">Imports and ORM singleton</h3>
<p>Circumflex ORM is packaged inside a single package. You should import all
  it's contents if you plan to use it in your code:</p>
<pre>${'import ru.circumflex.orm._'}</pre>
<p>Another notable and handy construction is the <em>ORM singleton</em>.
  It contains handy methods and helpers for transaction management,
  querying and some useful implicit converions. You should import it's contents
  to enjoy neat syntactic features:</p>
<pre>${'import ORM._'}</pre>
<p>In order to save us some typing time we omit import statements in source code
  fragments throughout this book, assuming you have these imports in place
  where necessary.</p>
<h3 id="src">Working with Circumflex ORM sources</h3>
<p>It is recommended that you have an up-to-date version of Circumflex sources
  (they could be obtained at <a href="http://github.com/inca/circumflex">GitHub</a>).
  Working with Circumflex sources will help you clarify different details, which are
  not fully covered in this documentation.</p>
<p>Please feel free to fork The Circumflex Project on GitHub and share with us any
  bug fixes or improvements you have in mind (or any thoughts on making Circumflex
  better). We highly appreciate your interest.</p>
[/@section]
[/@page]
