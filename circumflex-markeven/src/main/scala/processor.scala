package ru.circumflex
package markeven

import java.util.regex._
import java.io._
import collection.mutable.{HashMap, ListBuffer}

/*!# The Markeven Processor

`MarkevenProcessor` transforms text files into HTML using a set of simple rules.
`MarkevenProcessor` should be instantiated for every task and should not be shared
between threads or tasks to avoid orphaned data.

Markeven takes most ideas from [Markdown][], but has more strict rules, which lead to better
source structure and enhanced performance.

  [Markdown]: http://daringfireball.net/projects/markdown/syntax

# Syntax cheatsheet               {#syntax}

## Block elements

Markeven recognizes following block-level elements:

  * [paragraphs](#p);
  * [sections (or divs)](#div);
  * [headings](#hX);
  * [preformatted code blocks](#pre);
  * [ordered and unordered lists](#ol-ul);
  * [tables](#table);
  * [blockquotes](#blockquote);
  * [horizontal rulers](#hr);
  * [inline HTML](#inline-html)

Block elements are always delimited by two or more line ends (`\n\n`):

    This is a paragraph                                       {.no-highlight}

        this is a code block

### Paragraphs                         {#p}

A paragraph is simply one or more lines of text. Multiple lines are joined into a single paragraph:

    Paragraph one.                                             {.no-highlight}

    Paragraph two.
    More text.

    Paragraph three.
    More text.
    Even more text.

Following markup will be generated:

    <p>Paragraph one.</p>                                      {.html}
    <p>Paragraph two. More text.</p>
    <p>Paragraph three. More text. Even more text.</p>

Like in Markdown, if you wish to place linebreak, leave two or more space characters at the end of
the line.

### Sections (divs)                    {#div}

Section is a block which is rendered into an HTML `div` element. Each line of a section must start
with a pipe `|` character:

    | This is a section with two paragraphs.                      {.no-highlight}
    | I am the first one.
    |
    | And I am the second one.

Following markup will be generated:

    <div>                                                              {.html}
      <p>This is a section with two paragraphs.  I am the first one.</p>
      <p>And I am the second one.</p>
    </div>

Sections are frequently used in conjunction with [block selectors](#selectors) by web designers
to achieve certain effects like styling, animating, etc.

### Headings                                    {#hX}

Markeven supports both ATX and Setex styles proposed by Markdown:

    This is first-level heading                                        {.no-highlight}
    ===========================

    This is second-level heading
    ----------------------------

    # First level again

    ## Second level here

    ### Third level

    #### Fourth level

    ##### Fifth level

    ###### Sixth level

Unline Markdown, Markeven do not allow closing `#`s, so following example:

    # This is a heading which ends with #                               {.no-highlight}

will be transformed into:

    <h1>This is a heading which ends with #</h1>                        {.html}

### Preformatted code blocks                    {#pre}

Code blocks are used to write about programming or markup stuff. Their contents is usually
rendered using monospaced font and is interpreted literally. To produce a code block, indent
every line of block with at least 4 spaces or 1 tab:

    Here's some code:                                                   {.no-highlight}
    
        println("Hello world!")

Markeven will produce:

    <p>Here's some code:</p>                                            {.html}
    <pre><code>println("Hello world!")
    </code></pre>

You can indent only the first line of the code block. Note, however, that Markeven will trim at most
4 spaces in the beginning of each line.

You can also use GitHub like syntax to create fenced code blocks in case you find indenting each code line
cumbersome:

``` {.awesome-code}
def toHtml(str: String) = markeven.toHtml(str)
```

This will produce following markup:

```
<pre class="awesome-code"><code>def toHtml(str: String) =
    markeven.toHtml(str)</code></pre>
```

### Ordered and unordered lists                 {#ol-ul}

Lists in Markeven have strict rules which help you build highly structured documents.

The first thing to know about is _a list marker_. Ordered lists _must_ start with `1.`
followed by at least one space. Unordered lists _must_ start with `*` followed by
at least one space. Every subsequent list item must start with the same marker (a number
followed by a dot and whitespace in case of ordered lists):

    1. list item 1                                                     {.no-highlight}
    2. list item 2

    * list item 1
    * list item 2

Here is generated markup for the above snippet:

    <ol>                                                               {.html}
      <li>list item 1</li>
      <li>list item 2</li>
    </ol>
    <ul>
      <li>list item 1</li>
      <li>list item 2</li>
    </ul>

Lists items can contain another block-level elements. To interpret whitespace-sensitive blocks
properly, you should maintain the same indentation inside list items. We refer to this indentation
as _list item baseline_:

    *  This paragraph is under first list item.                          {.no-highlight}

       This paragraph is also under first list item, because
       it is properly indented.

    *     This list item has another baseline.

          So we should indent our second paragraph accordingly.

    This paragraph, however, is outside list.

Following markup will be generated:

    <ul>                                                                      {.html}
      <li>
        <p>This paragraph is under first list item.</p>
        <p>This paragraph is also under first list item, because it is properly indented.</p>
      </li>
      <li>
        <p>This list item has another baseline.</p>
        <p>So we should indent our second paragraph accordingly.</p>
      </li>
    </ul>
    <p>This paragraph, however, is outside list.</p>

Nested lists follow the same rules:

    1. List 1 item 1                                                        {.no-highlight}

    2. List 1 item 2

       1. List 2 item 1

       2. List 2 item 2

    3. List 1 item 3

Codeblocks can also be nested inside list items. Each line of a code block must be indented with
at least 4 spaces or 1 tab relatively to list item's baseline:

    1.  Code inside list item:                                             {.no-highlight}

            def sayHello = {
              println("Hello world!")
            }

You can also add a visual guide indicating current list item baseline using the pipe `|` character.
It can be useful in cases when the list item is long and its content is complex:

    1.   | This is a long and complex list item.                           {.no-highlight}
         |
         |    code block
         |
         |  * another list
         |  * ...

    2. And that's it.

Note that list items belong to the same list only if their markers are equaly indented. Following example
shows two different lists:

      * List one item one                                                   {.no-highlight}

      * List one item two

    * List two item one

    * List two item two

    And a paragraph.

Here's the markup:

    <ul>                                                                     {.html}
      <li>List one item one</li>
      <li>List one item two</li>
    </ul>
    <ul>
      <li>List two item one</li>
      <li>List two item two</li>
    </ul>
    <p>And a paragraph.</p>

### Tables                                        {#table}

Markeven supports simple syntax for tables:

    ---------------------------------------------                          {.no-highlight}
    |   Column 1  |   Column 2   |   Column 3   |
    --------------|--------------|---------------
    | one         | two          | three        |
    | four        | five         | six          |
    ---------------------------------------------

Here's the markup:

    <table>                                                              {.html}
      <thead>
        <tr>
          <th>Column 1</th>
          <th>Column 2</th>
          <th>Column 3</th>
        </tr>
      </thead>
      <tbody>
        <tr>
          <td>one</td>
          <td>two</td>
          <td>three</td>
        </tr>
        <tr>
          <td>four</td>
          <td>five</td>
          <td>six</td>
        </tr>
      </tbody>
    </table>

As you can see, the first and the last line of table should consist of minus `-` characters
only. The only exception to this rule is that the first line can optionally end with `>`
character. If `>` character is there, the width of table will expand to its maximum.

Cells are separated by the pipe `|` character, you can omit leading and trailing pipes.
Table header is separated from table body by the separator line. This line can optionally
contain semicolon `:` characters to express column alignment: a semicolon to the left side
means left alignment, a semicolon to the right side means right alignment, two semicolons
at both ends means center alignment:

    ---------------------------------------------                          {.no-highlight}
    |   Column 1  |   Column 2   |   Column 3   |
    -------------:|:------------:|:--------------
    |         one |      two     | three        |
    |        four |     five     | six          |
    ---------------------------------------------

You can also omit the header, in this case you cannot specify column alignment with semicolons:

    --------------------                                                   {.no-highlight}
    one  | two  | three
    --------------------

### Blockquotes                                   {#blockquote}

Blockquotes are similar to [sections](#sections), but they are rendered into HTML `blockquote`
element. Each line of a blockquote must start with `>` character. Like sections, blockquotes
can contain nested block elements:

    > This is blockquote.                                               {.no-highlight}
    >
    > > This blockquote is nested.
    >
    > That's it.

Here's generated markup:

    <blockquote>                                                          {.html}
      <p>This is blockquote.</p>
      <blockquote>This blockquote is nested.</blockquote>
      <p>That's it.</p>
    </blockquote>

### Horizontal rulers                             {#hr}

A horizontal ruler is rendered from a block which contains three or more minus `-` characters:

    This is some text.                                                  {.no-highlight}

    ---

    This is some more text.

Following markup will be produced by Markeven:

    <p>This is some text.</p>                                           {.html}
    <hr/>
    <p>This is some more text.</p>

No other syntaxes for `<hr/>` are supported.

### Inline HTML                                   {#inline-html}

Markeven allows you to place HTML elements right inside your text. Their content won't get processed:

    <div>                                                               {.no-highlight}

        This text won't get transformed into a code block

    </div>

        But this will.

There are no strict rules about inline HTML. The only important thing is that your markup should be
correct (tags closed and properly nested). Markeven does not have the ability to "fix" wrong HTML
markup yet :)

### Block selectors                               {#selectors}

Each block can optionally have a _selector_. It is used to add `id` and `class` HTML attributes
to blocks:

    I have an id.                     {#para1}                        {.no-highlight}

    I have two classes.               {.class1.class2}

    I have an id and a class.         {#para3.class1}

The example above will be transformed into a following HTML snippet:

    <p id="para1">I have an id.</p>                                   {.html}
    <p class="class1 class2">I have two classes.</p>
    <p id="para3" class="class1">I have an id and a class.</p>

The most common use of selectors is to assign `id` attribute so that they can be used in
[links](#links):

    Now I can be referenced by id!        {#mypara}                  {.no-highlight}

    Look! I can reference [another paragraph](#mypara).

The selector expression is enclosed into curly braces and must be placed at the end of the first line
of the block (no trailing whitespace allowed!).

## Text enhancements

Inside block level elements following text enhancements occur:

  * [links](#links) are processed;
  * text surrounded with backtick `` ` `` characters is transformed into `code` span;
  * text surrounded with underscores `_` becomes `em` (emphasized);
  * text surrounded with asterisks `*` becomes `strong` (strongly emphasized);
  * text surrounded with tildas `~` becomes `del` (deleted);
  * various typographic improvements are applied to text:

    * two minus chars `--` are replaces with -- (long tiret);
    * double quotes are replaced with "curly" quotes;
    * three consequtive dots `...` are replaced with ...;
    * `<-` and `->` are replaced with <- and -> accordingly;
    * `(c)`, `(r)` and `(tm)` are replaced with (c), (r), (tm);

You can also use backslash escaping to prevent misinterpreting special characters.
Following characters can be escaped: ```\`_*{}[]()#+-~.!```

## Links & Images

Two style of links are supported: inline and reference.

Inline links look like this: `[my text](http://my_url)` or `[some text](http://some_url "some title")`
and are rendered into HTML `a` element: `<a href="http://my_url">my text</a>` and
`<a href="http://some_url" title="some title">some text</a>`.

Reference-style links are split into link definition and link usage. Using previous examples, here's
how link definitions could look like:

    [id1]: http://my_url                                                   {.no-highlight}
    [id2]: http://some_url "some title"

Link usages would then look like this: `[my text][id1]` and `[some text][id2]`. The generated markup
is equal to the previous one.

The syntax for images is similar to the one for links: the exclamation `!` sign immediately before
opening bracket tells markeven to interpret the link as an image. Link text becomes the value of
`alt` attribute:

    Inline image: ![some image](/img/hello.png "Hello")                             {.no-highlight}

    Or reference-style image: ![some image][img]

      [img]: /img/hello.png "Hello"

Both cases generate following markup for image:

    <img src="/img/hello.png" title="Hello" alt="some image"/>
*/
class MarkevenProcessor() {

  val protector = new Protector
  val links = new HashMap[String, LinkDefinition]()
  var level = 0
  val macros = new HashMap[String, StringEx => CharSequence]()
  val postProcessors = new ListBuffer[(String, StringEx => StringEx)]()

  def increaseIndent() {
    level += 1
  }
  def decreaseIndent() {
    if (level > 0) level -= 1
  }

  def resolveLink(id: String): Option[LinkDefinition] = links.get(id.toLowerCase)

  def addMacro(name: String, function: StringEx => CharSequence): this.type = {
    macros += (name -> function)
    this
  }

  def postProcess(element: String)(handler: StringEx => StringEx) {
    this.postProcessors ++= List(element -> handler)
  }

  def currentIndent: String =
    if (level <= 0) ""
    else "  " * level

  def normalize(s: StringEx): StringEx = s.replaceAll("\t","    ").replaceAll(regexes.lineEnds, "\n")

  def cleanEmptyLines(s: StringEx): StringEx = s.replaceAll(regexes.blankLines, "")

  def stripLinkDefinitions(s: StringEx): StringEx = s.replaceAll(
    regexes.linkDefinition, m => {
      val id = m.group(1).trim.toLowerCase
      val url = processUrl(m.group(2))
      var t = m.group(4)
      val title = new StringEx(if (t == null) "" else t)
      encodeChars(title)
      encodeBackslashEscapes(title)
      encodeChars(url)
      encodeBackslashEscapes(url)
      links += id -> new LinkDefinition(url, title)
      ""
    })

  def hashHtmlBlocks(s: StringEx): StringEx =
    s.replaceIndexed(regexes.inlineHtmlBlockStart, m => {
      val startIdx = m.start
      var endIdx = 0
      if (m.group(2) != null) {
        // self-closing tag, escape as is
        endIdx = m.end
      } else {
        // find end-index of matching closing tag
        val tagName = m.group(1)
        // following regex will have `group(1) == null` for closing tags;
        // `group(2)` determines if a tag is self-closing.
        val tm = regexes.htmlTag(tagName).matcher(s.buffer)
        var depth = 1
        var idx = m.end
        while (depth > 0 && idx < s.length && tm.find(idx)) {
          if (tm.group(1) == null) depth -= 1        // closing tag
          else if (tm.group(2) == null) depth += 1   // opening tag
          idx = tm.end
        }
        endIdx = idx
      }
      // add to protector and replace
      val key = protector.addToken(s.buffer.subSequence(startIdx, endIdx))
      (key, endIdx)
    })

  def hashInlineHtml(s: StringEx): StringEx = s.replaceAll(regexes.htmlTag,
    m => protector.addToken(m.group(0)))

  def hashHtmlComments(s: StringEx): StringEx = s.replaceAll(regexes.htmlComment,
    m => protector.addToken(m.group(0)))

  def readBlocks(s: StringEx): Seq[Block] = {
    val result = new ListBuffer[Block]()
    val chunks = new ChunkIterator(s.split(regexes.blocks))
    while (chunks.hasNext)
      result += readBlock(chunks)
    result
  }

  def readBlock(chunks: ChunkIterator): Block = {
    // get current chunk
    val s = chunks.next
    // strip selector if any
    val selector = stripSelector(s)
    // assume code block
    if (s.startsWith("    "))
      return processComplexChunk(chunks, new CodeBlock(s, selector),
        c => c.startsWith("    "))
    // trim any leading whitespace
    val indent = s.trimLeft()
    // do not include empty freaks
    if (s.length == 0) return EmptyBlock
    // assume fenced code block
    if (s.startsWith("```")) {
      val c = s.clone.substring(3)
      if (c.startsWith("\n"))
        c.substring(1)
      // self-contained block?
      if (isFenceEnd(c))
        return new CodeBlock(c, selector).fenced()
      var inside = true
      return processComplexChunk(chunks, new CodeBlock(c, selector).fenced(), { c =>
        val chunk = unprotect(c)
        if (!inside) false
        else {
          if (isFenceEnd(chunk))
            inside = false
          true
        }
      })
    }
    // assume unordered list and ordered list
    if (s.startsWith("* "))
      return processComplexChunk(chunks, new UnorderedListBlock(s, selector, indent),
        c => isUl(c, indent))
    if (s.startsWith("1. "))
      return processComplexChunk(chunks, new OrderedListBlock(s, selector, indent),
        c => isOl(c, indent))
    // assume blockquote and section
    if (s.startsWith(">"))
      if (s.matches(regexes.d_blockquote)) {
        return new BlockquoteBlock(s, selector)
      } else return new ParagraphBlock(s, selector)
    if (s.startsWith("|"))
      if (s.matches(regexes.d_div)) {
        return new SectionBlock(s, selector)
      } else return new ParagraphBlock(s, selector)
    // assume table, headings and hrs
    s.matches(regexes.d_table, m => {
      new TableBlock(s, selector)
    }) orElse s.matches(regexes.d_heading, m => {
      val marker = m.group(1)
      val body = m.group(2)
      new HeadingBlock(new StringEx(body), selector, marker.length)
    }) orElse s.matches(regexes.d_h1, m => {
      new HeadingBlock(new StringEx(m.group(1)), selector, 1)
    }) orElse s.matches(regexes.d_h2, m => {
      new HeadingBlock(new StringEx(m.group(1)), selector, 2)
    }) orElse s.matches(regexes.d_hr, m => {
      new HorizontalRulerBlock(selector)
    }) match {
      case Some(block: Block) => block
      case _ => // nothing matched -- paragraph
        new ParagraphBlock(s, selector)
    }
  }

  def processComplexChunk(chunks: ChunkIterator,
                          block: Block,
                          accept: StringEx => Boolean): Block = {
    var eob = false
    while (chunks.hasNext && !eob) {
      val c = chunks.peek
      if (accept(c)) {
        block.text.append("\n\n").append(c)
        chunks.next
      } else eob = true
    }
    block
  }

  def isFenceEnd(s: StringEx): Boolean = {
    s.trimRight()
    if (s.endsWith("```")) {
      s.substring(0, s.length - 3)
      if (s.endsWith("\n"))
        s.substring(0, s.length - 1)
      return true
    }
    false
  }

  def isOl(s: StringEx, indent: Int): Boolean = {
    val i = new CharIterator(s)
    while (i.hasNext && i.index < indent - 1)
      if (i.next != ' ') return false
    if (!i.hasNext) return false
    // first char must be digit or space
    var c = i.next
    if (c == ' ') return true
    if (!c.isDigit) return false
    // look for more digits or `. `
    while(i.hasNext) {
      c = i.next
      if (c == '.' && i.hasNext && i.peek == ' ') return true
      else if (!c.isDigit) return false
    }
    false
  }

  def isUl(s: StringEx, indent: Int): Boolean = {
    val i = new CharIterator(s)
    while (i.hasNext && i.index < indent - 1)
      if (i.next != ' ') return false
    if (!i.hasNext) return false
    // first char must be asterisk or space
    var c = i.next
    if (c == ' ') return true
    (c == '*' && i.hasNext && i.peek == ' ')
  }

  def stripSelector(s: StringEx): Selector = {
    var id = ""
    var classes = new ListBuffer[String]()
    s.replaceFirst(regexes.blockSelector, m => {
      val idSelector = m.group(1)
      val classesSelector = m.group(2)
      if (idSelector != null)
        id = idSelector.substring(1)
      if (classesSelector != null)
        classesSelector.split("\\.").foreach { cl =>
          if (cl != "")
            classes += cl
        }
      ""
    })
    new Selector(id, classes)
  }

  def process(cs: CharSequence, out: Writer) {
    val s = new StringEx(cs)
    normalize(s)
    stripLinkDefinitions(s)
    hashHtmlBlocks(s)
    hashHtmlComments(s)
    cleanEmptyLines(s)
    writeHtml(readBlocks(s), out)
  }

  def transform(s: StringEx): StringEx = {
    normalizeSpan(s)
    hashInlineHtml(s)
    doMacros(s)
    encodeChars(s)
    doCodeSpans(s)
    encodeBackslashEscapes(s)
    doInlineImages(s)
    doRefImages(s)
    doInlineLinks(s)
    doRefLinks(s)
    doSpanEnhancements(s)
    unprotect(s)
  }

  def normalizeSpan(s: StringEx): StringEx =
    s.trim().replaceAll("  \n", "<br/>\n").replaceAll("\n", " ")

  protected def processSingleMacro(m: Matcher): CharSequence = {
    var name = m.group(1)
    if (name == null) name = ""
    if (name.length > 0)
      name = name.substring(0, name.length - 1)
    val contents = new StringEx(m.group(2))
    val r = macros.get(name).map(f => f(contents)).getOrElse(m.group(0))
    r
  }

  def doMacros(s: StringEx): StringEx =
    s.replaceAll(regexes.macro, m => protector.addToken(processSingleMacro(m)))

  def doCodeSpans(s: StringEx) {
    s.replaceAll(regexes.codeSpan, m => {
      val s = new StringEx(m.group(2)).trim()
      // there can be protected content inside codespans, so decode them first
      unprotect(s)
      encodeChars(s)
      protector.addToken(s.append("</code>").prepend("<code>"))
    })
  }

  def encodeBackslashEscapes(s: StringEx): StringEx =
    s.replaceAll(regexes.backslashChar, m => {
      val c = m.group(0)
      escapeMap.getOrElse(c, c)
    })

  def doRefLinks(s: StringEx): StringEx = s.replaceAll(regexes.refLinks, m => {
    val linkText = m.group(1)
    var id = m.group(2)
    if (id == "") id = linkText
    id = id.trim
    val linkContent = new StringEx(linkText)
    doSpanEnhancements(linkContent)
    val result = resolveLink(id)
        .map(ld => ld.toLink(linkContent))
        .getOrElse(new StringEx(m.group(0)))
    doMacros(result)
    protector.addToken(result)
  })

  def doRefImages(s: StringEx): StringEx = s.replaceAll(regexes.refImages, m => {
    val altText = m.group(1)
    var id = m.group(2)
    if (id == "") id = altText
    id = id.trim
    val result = resolveLink(id)
        .map(ld => ld.toImageLink(altText))
        .getOrElse(new StringEx(m.group(0)))
    doMacros(result)
    protector.addToken(result)
  })

  def doInlineLinks(s: StringEx): StringEx = s.replaceAll(regexes.inlineLinks, m => {
    val linkText = m.group(1)
    val url = encodeChars(unprotect(processUrl(m.group(2))))
    var title = m.group(4)
    if (title == null) title = ""
    val linkContent = new StringEx(linkText)
    doSpanEnhancements(linkContent)
    val result = new LinkDefinition(url, new StringEx(title)).toLink(linkContent)
    doMacros(result)
    protector.addToken(result)
  })

  def doInlineImages(s: StringEx): StringEx = s.replaceAll(regexes.inlineImages, m => {
    val altText = m.group(1)
    val url = processUrl(m.group(2))
    var title = m.group(4)
    if (title == null) title = ""
    val result = new LinkDefinition(url, new StringEx(title)).toImageLink(altText)
    doMacros(result)
    protector.addToken(result)
  })

  def doSpanEnhancements(s: StringEx): StringEx = {
    doTypographics(s)
    recurseSpanEnhancements(s)
    s
  }

  protected def processUrl(url: CharSequence): StringEx = new StringEx(url)

  protected def recurseSpanEnhancements(s: StringEx): StringEx =
    s.replaceAll(regexes.spanEnhancements, m => {
      val element = m.group(1) match {
        case "*" => "strong"
        case "_" => "em"
        case "~" => "del"
        case _ => "span"
      }
      val content = new StringEx(m.group(2))
      recurseSpanEnhancements(content)
      new StringEx("<").append(element).append(">")
          .append(content).append("</").append(element).append(">")
    })

  def doTypographics(s: StringEx): StringEx = {
    s.replaceAll(regexes.ty_dash, typographics.dash)
    s.replaceAll(regexes.ty_larr, typographics.larr)
    s.replaceAll(regexes.ty_rarr, typographics.rarr)
    s.replaceAll(regexes.ty_trade, typographics.trade)
    s.replaceAll(regexes.ty_reg, typographics.reg)
    s.replaceAll(regexes.ty_copy, typographics.copy)
    s.replaceAll(regexes.ty_hellip, typographics.hellip)
    s.replaceAll(regexes.ty_ldquo, typographics.ldquo)
    s.replaceAll(regexes.ty_rdquo, typographics.rdquo)
    s
  }

  def unprotect(s: StringEx): StringEx =
    s.replaceAll(regexes.protectKey, m => {
      val key = m.group(0)
      protector.decode(key) match {
        case Some(cs) => unprotect(new StringEx(cs))
        case _ => key
      }
    })

  def writeHtml(blocks: Seq[Block], out: Writer) {
    blocks.foreach(b => if (b != EmptyBlock) {
      b.writeHtml(this, out)
      out.write(newLine)
    })
  }

  def formHtml(blocks: Seq[Block], indent: Boolean = false): StringEx = {
    val result = new StringEx("")
    if (indent) level += 1
    blocks.foreach(b =>
      if (b != EmptyBlock) result.append(b.toHtml(this)).append(newLine))
    if (indent) level -= 1
    result
  }

  def newLine: String = "\n"

  def toHtml(cs: CharSequence): String = {
    val out = new StringWriter(cs.length)
    process(cs, out)
    out.toString
  }

}

