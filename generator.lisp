(defpackage #:pjs-yaclml-generator
  (:use #:cl #:pjs-utils #:pjs-chtml-helpers)
  (:export #:generate-code))

(in-package #:pjs-yaclml-generator)

(defun relative-path (stub)
  (asdf:system-relative-pathname '#:pjs-yaclml stub))

(defparameter +generated-tags-path+
  (relative-path "generated-tags.lisp"))

(defparameter +generated-entities-path+
  (relative-path "entities.sexp"))

(defparameter +elements-file-path+
  (relative-path "section-index.html"))

(defparameter +elements-url+
  "http://developers.whatwg.org/section-index.html")

(defparameter +entities-file-path+
  (relative-path "named-character-references.html"))

(defparameter +entities-url+
  "http://developers.whatwg.org/named-character-references.html")

(defun fetch-url-mtime (url)
  (multiple-value-bind (reply status headers)
      (drakma:http-request url
			   :method :head)
    (declare (ignore reply))
    (unless (= status 200)
      (error "bad status ~w" status))
    (dolist (header headers)
      (when (eq (car header)
		:last-modified)
	(let* ((date-string (cdr header))
	       (sdt (simple-date-time:from-string date-string))
	       (utime (simple-date-time:to-universal-time sdt)))
	  (return utime))))))

(defun file-contents (path)
  (with-open-file (s path)
    (let (line)
      (apply #'concatenate
	     'string
	     (until-c (eq 'end-of-file
			  (setf line
				(read-line s nil 'end-of-file)))
	       (collect line))))))

(defun fetch-page (url path)
  (let* ((mtime (or (and (cl-fad:file-exists-p path)
			 (file-write-date path))
		    0))
	 (url-mtime (fetch-url-mtime url)))
    (if (or (zerop mtime)
	    (null url-mtime)
	    (< mtime url-mtime))
	(with-open-file (s path :if-exists :supersede :direction :output)
	  (multiple-value-bind (reply status) (drakma:http-request url)
	    (unless (= 200 status)
	      (error "couldn't fetch list"))
	    (write-sequence reply s)))
	;; else
	(file-contents path))))

(defclass tag ()
  ((name :initarg :name
	 :reader tag-name)
   (empty :initarg :empty
	  :reader tag-empty)
   (attributes :initarg :attrs
	       :reader tag-attributes)))

(defun parse-element-list-page (data)
  (let* ((*document* (chtml:parse data (chtml:make-lhtml-builder)))
	 (table (dom-find-if (dom-node-of-tag :table)))
	 (tbody (dom-find-if (dom-node-of-tag :tbody)
			     table))
	 (rows (dom-find-all-if (dom-node-of-tag :tr)
			    tbody)))
    (dolist-c (row rows)
      (destructuring-bind (element description categories parents children attributes interface) 
	  (dom-find-all-if (lambda (node)
			     (or (eq (dom-tag-kind node)
				     :th)
				 (eq (dom-tag-kind node)
				     :td)))
			   row)
	(declare (ignore description categories parents interface))
	(dolist (element-name (dom-find-all-if (dom-node-of-tag :a)
					       element))
	  (collect (make-instance 'tag 
				  :name (dom-node-text element-name)
				  :empty (string= (dom-text-node-p children)
						  "empty")
				  :attrs (dolist-c (attr (mapcar (lambda (node)
								   (dom-node-text (first (dom-children node))))
								 (dom-find-all-if (dom-node-of-tag :code)
									      attributes)))
					   (collect attr)))))))))

(defvar *indent* 0)

(defun o (format &rest args)
  (when (string= (subseq format 0 2)
		 "~&")
    (format t "~&")
    (setf format (subseq format 2)))
  (dotimes (i *indent*)
    (format t " "))
  (apply #'format t format args))

(defmacro with-indent* (increment &body body)
  `(let ((*indent* (+ *indent* ,increment)))
     ,@body))

(defmacro with-indent (&body body)
  `(with-indent* 2
     ,@body))

(defmacro with-output-file (path &body body)
  `(with-open-file (*standard-output* ,path :direction :output :if-exists :supersede)
     ,@body))

(defun generate-tag-code ()
  (let ((tags (parse-element-list-page (fetch-page +elements-url+
						   +elements-file-path+))))
    (with-output-file +generated-tags-path+ 
      (o "(defpackage #:<~%")
      (with-indent 
	(o "(:use)~%")
	(o "(:import-from #:pjs-yaclml #:as-html #:as-is)~%")
	(o "(:export #:as-html")
	(with-indent* 9
	  (o "~&#:as-is")
	  (dolist (tag tags)
	    (o "~&#:~a" (tag-name tag)))))
      (o "))~%~%")
      (o "(in-package #:pjs-yaclml)~%~%")
      (dolist (tag tags)
	(o (if (tag-empty tag)
	       "(def-std-tag <:~a t"
	       ;; else
	       "(def-std-tag <:~a nil")
	   (tag-name tag))
	(dolist (attr (tag-attributes tag))
	  (o "~&  ~a" attr))
	(o ")~%~%")))))

(defclass entity ()
  ((name :initarg :name
	 :reader entity-name)
   (chars :initarg :chars
	  :reader entity-chars)))

(defun parse-unicode-code (str)
  (unless (string= "U+" (subseq str 0 2))
    (error "invalid unicode string ~w" str))
  (code-char (parse-integer (subseq str 2)
			    :radix 16)))

(defun parse-unicode-column (column)
  (let* ((str (dom-node-text column))
	 (str (string-trim '(#\Space #\Tab #\Return #\Linefeed)
			   str))
	 (code-points (explode " " str))
	 (code-points (mapcar #'parse-unicode-code code-points)))
    code-points))

(defun parse-entity-list-page (data)
  (let* ((*document* (chtml:parse data (chtml:make-lhtml-builder)))
	 (table (dom-find-if (dom-node-of-tag :table)))
	 (tbody (dom-find-if (dom-node-of-tag :tbody) table))
	 (rows (dom-find-all-if (dom-node-of-tag :tr) tbody)))
    (dolist-c (row rows)
      (let* ((columns (dom-children row))
	     (name-column (first columns))
	     (name-code (dom-find-if (dom-node-of-tag :code)
				     name-column))
	     (name (string-right-trim '(#\;) (dom-node-text name-code)))
	     (unicode-column (second columns))
	     (chars (parse-unicode-column unicode-column)))
	(collect (make-instance 'entity
				:name name
				:chars chars))))))

(defun generate-entity-code ()
  (let ((entities (parse-entity-list-page (fetch-page +entities-url+
						      +entities-file-path+))))
    (multiple-value-bind (ones twos)
	(partition (lambda (entity)
		     (= (length (entity-chars entity))
			1))
		   entities)
      (with-output-file +generated-entities-path+
	(format t "; entities ~w~%" (length entities))
	(format t ";; single char entities ~w~%" (length ones))
	(format t  "(")
	(dolist (entity ones)
	  (format t "~w ~a~%" (entity-name entity)
		  (char-code (first (entity-chars entity)))))
	(format t ")~%;; double-char entities ~w~%" (length twos))
	(format t "(")
	(dolist (entity twos)
	  (let ((chars (entity-chars entity)))
	    (format t "~w ~a ~a~%"
		    (entity-name entity)
		    (char-code (first chars))
		    (char-code (second chars)))))
	(format t ")~%")))))

(defun generate-code ()
  (generate-entity-code)
  (generate-tag-code))
