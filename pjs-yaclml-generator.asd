(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(#:drakma #:cl-fad #:simple-date-time #:closure-html)))

(asdf:defsystem #:pjs-yaclml-generator
  :serial t
  :depends-on (#:pjs-utils #:pjs-chtml-helpers)
  :components ((:file "generator")))
