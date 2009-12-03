[#ftl]
[#include "./layout.ftl"]
[@page]
[@section id="circumflex-orm"
          theme="paper"
          title="Circumflex ORM"]
<p>Circumflex ORM (a.k.a. <em>circumflex-orm</em>) is a lightweight Object-Relational
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
[@follow/]
[/@section]
[/@page]
