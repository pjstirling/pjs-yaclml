(asdf:defsystem #:pjs-yaclml
  :serial t
  :depends-on (#:pjs-utils)
  :components ((:file "package")
	       (:file "helpers")
               (:file "pjs-yaclml")
	       (:file "generated-tags")))
