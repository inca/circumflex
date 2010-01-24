[#ftl]
[#include "/layout.ftl"]
[@page]
[@section id="data-model" title="Chapter 3. Creating Data Model"]
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
  <a href="#relations">relations</a>, <a href="#records">records</a>
  and <a href="#aux">auxiliary database objects</a>. They allow
  fine-grained control over resulting DDL (data definition language)
  statements that are used to create real database schema for your
  application. The syntax for object definition closely resembles SQL, so it
  could be read even by those unfamiliar with Scala. These objects
  are subsequently used in your applocation to deal with all data-related
  operations.</p>
<p>It is a common practice to group such objects in a single Scala package
  (it is not required, but usually is conveniant, for example, for generating
  database schema):</p>
<pre>${'package ru.circumflex.sandbox.model'?html}</pre>
<p>Let's take a close look at the essential components of Circumflex ORM.</p>
<h2 id="relations">Relations</h2>
<p>In relational theory, a <em>relation</em> is a data structure which
  consists of a heading and an unordered set of tuples which share the same
  type.</p>
<p>In Circumflex ORM a relation is an object, whose type is derived from
  the <code>Relation</code> class. Relations are used to store and retrieve
  records and usually correspond to database <em>tables</em> and
  <em>views</em>.</p>
<p></p>
[/@section]
[/@page]
