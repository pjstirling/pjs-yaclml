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
  "https://html.spec.whatwg.org/dev/indices.html")

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

(defun fetch-page (url path use-cached)
  (if (and use-cached
	   (probe-file path))
      (file-contents path)
      ;; else
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
	    (file-contents path)))))

(defclass tag ()
  ((name :initarg :name
	 :reader tag-name)
   (empty :initarg :empty
	  :reader tag-empty)
   (attributes :initarg :attrs
	       :reader tag-attributes)))

(defun dom-deep-node-text (node)
  (let (parts)
    (labels ((handle (node)
	       (dolist (child (dom-children node))
		 (if (stringp child)
		     (push child parts)
		     (handle child)))))
      (handle node)
      (apply #'concatenate
		'string
		(nreverse parts)))))

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
	(dolist (element-name (dom-find-all-if (dom-node-of-tag :code)
					       element))
	  (collect (make-instance 'tag 
				  :name (dom-deep-node-text element-name)
				  :empty (string= (dom-text-node-p children)
						  "empty")
				  :attrs (dolist-c (attr (mapcar (lambda (node)
								   (dom-node-text (first (dom-children node))))
								 (dom-find-all-if (dom-node-of-tag :code)
									      attributes)))
					   (collect attr)))))))))

(defmacro with-output-file (path &body body)
  `(with-open-file (*standard-output* ,path :direction :output :if-exists :supersede)
     ,@body))

(defun output-tag-code (tags)
  (with-output-file +generated-tags-path+
    (let ((*print-case* :downcase)
	  (*package* (find-package '#:pjs-yaclml-generator)))
      (print `(defpackage #:<
		(:use)
		(:import-from #:pjs-yaclml #:as-html #:as-is #:!doctype-html)
		(:export #:as-html
			 #:as-is
			 #:!doctype-html
			 ,@ (mapcar #'make-symbol
				    (mapcar #'string-upcase
					    (mapcar #'tag-name tags))))))
      (format t "~%~%(in-package #:pjs-yaclml)~%~%")
      (dolist (tag tags)
	(pprint-logical-block (*standard-output* nil :prefix "(" :suffix ")")
	  (format t "def-std-tag <:~a ~a" (tag-name tag) (not (not (tag-empty tag))))
	  (dolist (attr (tag-attributes tag))
	    (format t "~&  #:~a" attr)))
	(format t "~%~%")))))

(defun generate-tag-code (use-cached)
  (let ((tags (parse-element-list-page (fetch-page +elements-url+
						   +elements-file-path+
						   use-cached))))
    (output-tag-code tags)))

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

(defun output-entity-code (entities ones twos)
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
    (format t ")~%")))

(defun generate-entity-code (use-cached)
  (let ((entities (parse-entity-list-page (fetch-page +entities-url+
						      +entities-file-path+
						      use-cached))))
    (multiple-value-bind (ones twos)
	(partition (lambda (entity)
		     (= (length (entity-chars entity))
			1))
		   entities)
      (output-entity-code entities ones twos))))

(defun generate-code (&optional use-cached)
  (generate-entity-code use-cached)
  (generate-tag-code use-cached))
