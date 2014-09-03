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

```
(yaclml:with-yaclml-output-to-string
  (<:as-is "<!DOCTYPE html>" #\Newline)
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