[#ftl]
[#include "./layout.ftl"]
[@page]

[@section id="circumflex" theme="paper" title="Introducing Circumflex"]
<p>Circumflex is a set of mini-frameworks and tiny helpers for quick and robust application
   development using <a href="http://scala-lang.org" target="_blank">Scala programming language</a>.
</p>
<p>Circumflex features two mini-frameworks:</p>
<ul>
  <li><a href="#circumflex-core">Circumflex Web Framework</a></li>
  <li><a href="#circumflex-orm">Circumflex ORM Framework</a></li>
</ul>
<p>These framework provide Domain-Specific Languages that make the development process
   natural and extremely productive.</p>
<p>Circumflex does not contain a comprehensive features list for every task a developer
   might ever need; instead it allows developers to choose the tools and libraries that
   best suit their particular needs.</p>
<p>Circumflex is designed to use the powers of Scala programming language and
  <a href="http://maven.apache.org" target="_blank">Apache Maven 2</a> software management
   platform – these are the only assumptions Circumflex makes about your applications.</p>
<p>Circumflex has several key concepts.</p>
<ul>
  <li>The application should not be burried in an endless pile of configuration files, that
      are supposed to be maintained by non-programmer kinds. Circumflex components require minimum
      initial configuration, while still allowing developers to easily override defaults if necessary.</li>
  <li>Scala applications are easily integrated with Java libraries, but it is often very
      inconveniant to use them from Scala code. Circumflex helpers for popular Java tools are
      intended to eliminate these complexities.</li>
  <li>All Circumflex components are relied on Scala support for Domain-Specific Languages (DSLs).
      They can make the development process more natural and effective.</li>
</ul>
[/@section]

[@section id="circumflex-core"
          theme="paper"
          title="Circumflex Web Framework"]
<p>Circumflex Web Framework (a.k.a. <em>circumflex-core</em>) is a lightweight Front Controller
   pattern implementation for quick and robust Web application development.</p>
<p>Here is Circumflex hello-world application:</p>
<pre id="hello.scala">
package myapp

import ru.circumflex.core.RequestRouter

class Main extends RequestRouter {
  get("/") = "Hello world!"
}
</pre>
<p>Circumflex Web Framework does not rely on specific view technology, however Circumflex
   comes shipped with a helper for <a href="http://freemarker.org" target="_blank">FreeMarker</a>,
   a powerful feature-rich generic template engine. Circumflex FreeMarker Helper (a.k.a.
  <em>circumflex-ftl</em>) allows developers to use Scala core types as template's data model to
   easily create full-fledged web applications.</p>
<p>For example, the following template (<em>test.ftl</em>):</p>
<pre>${r"
[#ftl]
<ul>
  [#list myList as elem]
  <li>${elem}</li>
  [/#list]
</ul>"?html}
</pre>
<p>and following code:</p>
<pre id="test.scala">${r'
package myapp

import ru.circumflex.core._
import ru.circumflex.freemarker._

class Main extends RequestRouter with FreemarkerHelper {
  get("/test") = {
    ctx += "myList" -> List("one", "two", "three")
    ftl("test.ftl")
  }
}
'?html}</pre>
<p>will result in following markup:</p>
<pre>${r'
<ul>
    <li>one</li>
    <li>two</li>
    <li>three</li>
</ul>'?html}</pre>
<p>The power and flexibility of Scala combined with various Circumflex helpers makes the
   development of arbitrary complex applications almost that simple, too.</p>
[/@section]

[@section id="circumflex-orm"
          theme="paper"
          title="Circumflex ORM Framework"]
<p>Circumflex ORM Framework (a.k.a. <em>circumflex-orm</em>) is a lightweight Object-Relational
   Mapping framework for Scala-based applications. It features schema generation, complex querying
   and dialect support, while eliminating meta-data, configuration complexity, transaction
   demarcation and other headaches of popular ORM's.</p>
<p>The key concept of Circumflex ORM is the <em>clarity</em>. Most popular ORMs try to hide SQL
   details from a developer, making them somewhat "leaky abstractions": you cannot deal with
   persistence well unless you know exactly what you are doing. Circumflex ORM let's you specify
   the exact behavior in very conveniant DSL-like fashion.</p>
<p>Domain model validation is also a part of Circumflex ORM.</p>
<p>Consider the following example:</p>
<pre>
  package myapp

  import ru.circumflex.orm._

  class Category extends Record[Category] {
    def relation = Category
    val id = field(Category.id)
    val name = field(Category.name)
    val books = oneToMany(Book.category)    // helps to fetch books lazily
  }

  object Category extends GenericTable[Category] {
    val name = stringColumn("name")         // creates a column
        .notNull                            // creates NOT NULL constraint
        .unique                             // creates UNIQUE constraint
        .validateNotEmpty                   // adds NotEmpty validation
        .validatePattern("^[a-zA-Z]{1,8}$") // adds Pattern validation
  }

  class Book extends Record[Book] {
    def relation = Book
    val id = field(Book.id)
    val title = field(Book.title)
    val category = manyToOne(Book.category)
  }

  object Book extends GenericTable[Book] {
    val title = stringColumn("title")
        .notNull
        .validateNotEmpty
    val category = longColumn("category_id")
        .references(Category)     // creates an association with Category
        .onDeleteSetNull          // specifies a foreign-key action
        .onUpdateCascade
  }
</pre>
<p>This is a typical domain model definition using Circumflex ORM. As you can see, there are
   two kinds of top-level definitions, <em>records</em> and <em>tables</em>: tables describe,
   how records will be queried and persisted. This information is quite enough to perform all
   common operations:</p>
<ul>
  <li>schema generation:
    <pre>new DDLExport(Category, Book).create   // creates database schema</pre>
  </li>
  <li>simple querying:
    <pre>
// find category by id
val c = Category.get(2l)
// find all books
val allBooks = Book.all
// find books for category
val cBooks = c.get.books
// find books by title
Book.createCriteria.add("title" like "a%").list
</pre></li>
  <li>querying for arbitrary tuples:
    <pre>
select(Category as "c" join (Book as "b"), Category as "c1")
      .where("c1.name" like "a%")
      .addOrder(asc("c.name"))
      .list
// generates following SQL:
select
	c.id as c_id,
	c.name as c_name,
	b.id as b_id,
	b.title as b_title,
	b.category_id as b_category_id,
	c1.id as c1_id,
	c1.name as c1_name
from
	public.category as c
		left join public.book as b
			on (c.id = b.category_id),
	public.category as c1
where
	c1.name like ?
order by
	c.name asc
</pre></li>
  <li>inserting, updating, validating and deleting records:
    <pre>
val cat = new Category
cat.name := "New category"
cat.save    // validates and issues INSERT statement
cat.name := "Romances"
cat.save    // validates and issues UPDATE statement
cat.delete  // issues DELETE statement</pre></li>
</ul>
[/@section]
[/@page]
