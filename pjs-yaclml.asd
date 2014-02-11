(asdf:defsystem #:pjs-yaclml
  :serial t
  :depends-on (#:pjs-utils)
  :components ((:file "generated-package")
	       (:file "package")
	       (:file "helpers")
               (:file "pjs-yaclml")
	       (:file "generated-tags")))
