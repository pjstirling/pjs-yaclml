pjs-yaclml
==========

Yes, this is a brilliantly original name for a library.

YACLML is an html-generation library written by Edward Marco Baringer,
but it has seen no active development in several years. In the gap has
arisen the WHATWG and html5, which adds new elements, and some tweaks to
the parsing algorithm for html (in particular you can't use xml-style
empty tags to open and close a content-enabled element, leading to mass
horrible-ness).

I decided to write my own version (how hard could it be?), rather than
attempt to fork the original, or port my own projects to a different
library. This library is the result. 

Getting started
===============

Before you use this library the first time you will need to fetch the
up-to-date list of elements and their attributes from the WHATWG spec,
by loading the system PJS-YACLML-GENERATOR and invoking
(PJS-YACLML-GENERATOR:GENERATE-CODE).

This only needs to be repeated if you want to update against any
changes to the spec. e.g. when a new element is added.

Example usage (once the package PJS-YACLML has been loaded):

```common-lisp
(yaclml:with-yaclml-output-to-string
  (<:!doctype-html)
  (<:html
    (<:head
      (<:title "Hi there!")
      (<:link :rel "stylesheet" :href (sconc +server-prefix+ "style.css") :type "text/css"))
    (<:body
      (function-that-outputs-html)
      (<:ul
	(dotimes (i 10)
	  (<:li (<:as-html i))))))))
      
```

Slightly Longer
===============

YACLML uses a special variable to choose where to send its output, and provides two ways to bind it:

1. (PJS-YACLML:WITH-YACLML-OUTPUT-TO-STRING (&body body)) This returns all output as a string.
2. (PJS-YACLML:WITH-YACLML-STREAM (stream &body body)) This sends all output to the provided stream.

There are several ways to send output to YACLML, these symbols all live in the "<" package (which is intended to evoke html syntax, I LOVE common-lisp naming flexibility):

1. (<:!doctype-html) Outputs the html5 doctype
2. (<:AS-IS (&rest forms)) This PRINCs each form unchanged to output
3. (<:AS-HTML (&rest forms)) This first PRINCs (if required) each form to produce a string, then html-encodes it, before sending it to output.
4. An html form; by which is meant a lisp form starting with a symbol, from the "<" package, that names an html element. Attributes may be specified by keyword-value pairs, For boolean attributes use T as the value. If the value provided for an attribute is NIL it will be completely absent from the output.
5. Strings lexically within an html form will be interpreted as if they were inside an <:AS-HTML form.

YACLML will attempt to aggressively coalesce output calls by macroexpanding each sub-form and flattening PROGNs. You may access this functionality by wrapping code in a PJS-YACLML:HTML-BLOCK form.

