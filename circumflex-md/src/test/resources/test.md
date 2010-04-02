Block elements
====================

Inline HTML
-----------

<div style="border:1px solid red">
  <div id="one">
    <div id="one-one">
    </div>
    <div id="one-two">
    </div>
  </div>
  <div id="two">
  </div>
</div>

## HTML comments

<!-- This is the comment and should be left AS IS. -->

## Rulers

* * *

---------------------------------

_ _ _

## Lists

A paragraph.

 *  Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam eleifend,
    est nec tincidunt iaculis, sapien erat dignissim odio, quis placerat nisl
    felis a leo. Vestibulum ornare, lacus ac dapibus luctus, orci nisl fermentum
    tellus, eget convallis leo tortor vitae orci. Nam vestibulum venenatis
    lectus a viverra.

    Quisque commodo mauris eget nunc dapibus sit amet tristique elit scelerisque.
    Suspendisse accumsan est quis nunc feugiat facilisis. Aenean pharetra imperdiet
    felis eget pulvinar. Vivamus fringilla erat sit amet tellus tristique semper.
    Proin sagittis eleifend aliquet. Aenean porta, nulla ut adipiscing gravida,
    nibh ipsum commodo leo, eget imperdiet lorem nisl quis felis.

 *  Pellentesque eros felis, feugiat a sagittis id, fringilla sed ante. Nam vehicula
    lorem a tellus pulvinar rutrum pellentesque elit congue.

 *  Interesting, what if...

    *  wow, we have sub-list in here

    *  and here, too!!

    *  and a blockquote:
    
        > and here we got blockquotes in lists
        > and still a blockquote

    What about some para?

 *  Can we please go back to that outer list?

Donec magna orci, tincidunt eget semper a, pellentesque vel massa. Ut et justo purus.
Fusce at sapien sit amet dui ultrices eleifend. Praesent pulvinar purus eu ante congue
euismod eget non orci. Vestibulum placerat, risus vitae adipiscing eleifend,
purus dolor aliquet neque, id ullamcorper elit diam et massa. Donec rutrum varius
augue in volutpat. Cras imperdiet porta purus nec pellentesque. Morbi sed erat lorem,
ut dignissim enim. Nullam et neque nec turpis egestas ultrices. Donec pretium, elit
at egestas egestas, nisi arcu adipiscing nibh, vitae condimentum nisl nunc non risus.

1. and
2. a
3. tiny
8. ordered
5. list

Code blocks
-----------

Let's look at one:

    lang:scala
    def addToken(t: CharSequence): String = unprotectHash.get(t) match {
      case Some(key) => key
      case _ =>
        val key = randomKey
        protectHash += key -> t
        unprotectHash += t -> key
        key
    }

That's it. See, it's way too easy.

Block quotes
------------

Let's try it out.

> Donec magna orci, tincidunt eget semper a, pellentesque vel massa. Ut et justo purus.
Fusce at sapien sit amet dui ultrices eleifend. Praesent pulvinar purus eu ante congue
euismod eget non orci. Vestibulum placerat, risus vitae adipiscing eleifend,
purus dolor aliquet neque, id ullamcorper elit diam et massa. Donec rutrum varius
augue in volutpat. Cras imperdiet porta purus nec pellentesque. Morbi sed erat lorem,
ut dignissim enim. Nullam et neque nec turpis egestas ultrices. Donec pretium, elit
at egestas egestas, nisi arcu adipiscing nibh, vitae condimentum nisl nunc non risus.

> ### Continuing...
>
> Donec magna orci, tincidunt eget semper a, pellentesque vel massa. Ut et justo purus.
> Fusce at sapien sit amet dui ultrices eleifend. Praesent pulvinar purus eu ante congue
> euismod eget non orci. Vestibulum placerat, risus vitae adipiscing eleifend,
> purus dolor aliquet neque, id ullamcorper elit diam et massa. Donec rutrum varius
> augue in volutpat. Cras imperdiet porta purus nec pellentesque. Morbi sed erat lorem,
> ut dignissim enim. Nullam et neque nec turpis egestas ultrices. Donec pretium, elit
> at egestas egestas, nisi arcu adipiscing nibh, vitae condimentum nisl nunc non risus.

Aaaand that's it. See?

Now weird stuff
---------------
We've discovered that things like this: <a href="<a href='<a>`_*& &amp;</a>'></a>"></a>
are possible. Let's see if we treat this baby carefully.

Span elements
=============

Code spans
----------

We have `` some code with ticks (`) in here ``!!1

Images
------

Let's try it with titles: ![Alt text](/path/to/img.jpg "Optional title")

Now let's try it without ones: ![Alt text](/path/to/img.jpg)

## Links

I get 10 times more traffic from [Google] [1] than from
[Yahoo] [2] or [MSN] [3].

  [1]: http://google.com/        "Google"
  [2]: http://search.yahoo.com/  "Yahoo Search"
  [3]: http://search.msn.com/    "MSN Search"

  * * * * *

I get 10 times more traffic from [Google][] than from
[Yahoo][] or [MSN][].

  [google]: http://google.com/
  [yahoo]:  http://search.yahoo.com/
  [msn]:    http://search.msn.com/

Now let's taste some <http://autolinks.com> and <emails@thingies.net>.






