(defpackage #:pjs-yaclml
  (:use #:cl #:pjs-utils)
  (:nicknames #:yaclml)
  (:export #:with-yaclml-output-to-string
	   #:with-yaclml-stream
	   #:*yaclml-stream*

	   #:deftag
	   #:def-std-tag
	   #:output-lhtml
	   #:as-html
	   #:as-is
	   #:html-block	   
	   #:custom-attr
	   #:tag-info
	   #:tag-info-name
	   #:tag-info-attrs
	   #:tag-info-empty
	   #:*tag-info*))

(in-package #:pjs-yaclml)

(defvar *yaclml-stream* nil)

(defparameter +global-attributes+
  '(accesskey class contenteditable contextmenu dir draggable dropzone hidden id inert itemid itemprop itemref itemscope itemtype lang spellcheck style tabindex title translate
    onabort onblur oncancel oncanplay oncanplaythrough onchange onclick onclose oncontextmenu oncuechange ondblclick ondrag ondragend ondragenter ondragleave ondragover ondragstart ondrop ondurationchange onemptied onended onerror onfocus oninput oninvalid onkeydown onkeypress onkeyup onload onloadeddata onloadedmetadata onloadstart onmousedown onmousemove onmouseout onmouseover onmouseup onmousewheel onpause onplay onplaying onprogress onratechange onreset onscroll onseeked onseeking onselect onshow onsort onstalled onsubmit onsuspend ontimeupdate onvolumechange onwaiting))

(defvar *tag-info* (make-hash-table :test 'equal))

(defparameter *double-char-entity-map* nil)
(defparameter *entity-char-map* nil)
(defparameter *char-entity-map* nil)

(defparameter +safe-char-list+
  (sort (copy-seq #(#\. #\: #\/ #\\ #\= #\! #\, #\( #\) #\Newline))
	#'char<))
