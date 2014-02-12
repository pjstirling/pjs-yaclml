(in-package #:pjs-yaclml)

(defmacro with-yaclml-output-to-string (&body body)
  `(with-output-to-string (*yaclml-stream*)
     ,@body))

(defmacro with-yaclml-stream (stream &body body)
  `(let ((*yaclml-stream* ,stream))
     ,@body))

(defmacro <:as-html (&environment env &rest contents)
  (let ((contents (mapcar (lambda (form)
			    (let ((form (macroexpand form env)))
			      `(<ai ,(if (compile-time-constant form)
					 (escape-as-html form)
					 ;; else
					 `(escape-as-html ,form)))))
			  contents)))
    `(progn
       ,@(merge-ai contents))))

(defmacro <ah (&rest rest)
  `(<:as-html ,@rest))

(defun <:as-is (&rest args)
  (dolist (arg args)
    (<ai arg)))

(defclass tag-info ()
  ((name :initarg :name
	 :reader tag-info-name)
   (attrs :initarg :attrs
	  :reader tag-info-attrs)
   (empty :initarg :empty
	  :reader tag-info-empty)))

(defmethod print-object ((info tag-info) stream)
  (let ((*standard-output* stream))
    (print-unreadable-object (info t)
      (princ "tag-info")
      (princ (tag-info-name info))
      (princ "attrs: ")
      (princ (tag-info-attrs info))
      (princ "empty:")
      (princ (tag-info-empty info)))))

(defun attrs-and-contents (attr-names contents)
  (with-collector* (collect)
    (let (completed)
      (while (and contents
		  (not completed))
	(let* ((first (first contents))
	       (name (and (keywordp first)
			  (symbol-name* first))))
	  (if (and name
		   (member name
			   attr-names
			   :test #'string=))
	      (progn
		(pop contents)
		(collect (list name
			       (pop contents))))
	      ;; else
	      (if (and (listp first)
		       (eq (first first)
			   'custom-attr))
		  (progn
		    (collect (list (second first)
				   (third first)))
		    (pop contents))
		  ;; else
		  (setf completed t)))))
      (values (collect) contents))))

(defun deftag-template (name empty-tag attrs body)
  (let ((attrs (remove-if #'null attrs :key #'second)))
    (multiple-value-bind (compile-time-attrs run-time-attrs)
	(partition (lambda (attr)
		     (compile-time-constant (second attr)))
		   attrs)
      `(progn
	 (<ai ,(sconc "<" (symbol-name* name)))
	 ,@(mapcar (lambda (attr)
		     `(progn
			(<ai ,(sconc " " (first attr)))
			,(unless (eq t (second attr))
			   `(<ai ,(sconc "=\""
					 (escape-as-html (second attr))
					 "\"")))))
		   compile-time-attrs)
	 ,@(mapcar (lambda (attr)
		     `(awhen ,(second attr)
			(<ai ,(sconc " " (first attr)))
			(unless (eq t it)
			  (<ai "=\"")
			  (<ai (escape-as-html it))
			  (<ai "\""))))
		   run-time-attrs)
	 (<ai ">")
	 ,(if empty-tag
	      (when body
		(error "body provided for void element ~a ~a" name body))
	      ;; else
	      `(progn
		 ,@(mapcar (lambda (form)
			     (if (compile-time-constant form)
				 `(<ai ,(escape-as-html form))
				 ;; else
				 form))
			   body)
		 (<ai ,(sconc "</" (symbol-name* name) ">"))))))))


(defmacro deftag (name empty-tag &body attrs)
  (let ((attrs (remove-duplicates (append +global-attributes+ attrs))))
    `(progn
       (setf (gethash ,(symbol-name name) *tag-info*)
	     (make-instance 'tag-info
			    :name ,(symbol-name name)
			    :attrs ',(mapcar #'symbol-name attrs)
			    :empty ,empty-tag))
       (defmacro ,name (&environment env &body contents)
	 (multiple-value-bind (attrs contents)
	     (attrs-and-contents ',(mapcar #'symbol-name* attrs)
				 (mapcar (lambda (form)
					   (macroexpand form env))
					 contents))
	   (let* ((template (deftag-template ',name
			      ,empty-tag
			      attrs
			      contents))
		  (merged-progns (merge-progns template))
		  (merged-ai (merge-ai merged-progns)))
	     merged-ai))))))

(defun output-unhandled-lhtml-node (node-name)
  (<ai "<!-- unhandled tag kind ")
  (<ah node-name)
  (<ai "-->"))

(defun output-lhtml-attr (attr tag-info error-stream)
  (destructuring-bind (name val) attr
    (if (member name
		(tag-info-attrs tag-info)
		:test #'string=)
	(progn
	  (<ai " ")
	  (<ai name)
	  (unless (eq t val)
	    (<ai "=\"")
	    (<ah val)
	    (<ai "\"")))
	;; else
	(progn
	  (with-yaclml-stream error-stream
	    (<ai "<!-- unhandled attribute ")
	    (<ai name)
	    (<ai " -->"))))))

(defun output-handled-lhtml-node (node-name attrs children tag-info)
  (let ((error-stream (make-string-output-stream)))
    (<ai "<")
    (<ai node-name)
    (dolist (attr attrs)
      (output-lhtml-attr attr tag-info error-stream))
    (if (tag-info-empty tag-info)
	(progn
	  (when children
	    (with-yaclml-stream error-stream
	      (<ai "<!-- skipped children for ")
	      (<ai node-name)
	      (<ai " -->")))
	  (<ai " />"))
	;; else
	(progn
	  (<ai ">")
	  (dolist (child children)
	    (if (and (string= node-name "SCRIPT")
		     (stringp child))
		(<ai child)
		;; else
		(output-lhtml child)))
	  (<ai "</")
	  (<ai node-name)
	  (<ai ">")))
    (<ai (get-output-stream-string error-stream))))

(defun output-lhtml-node (node)
  (unless node
    (error "node is null"))
  (destructuring-bind (node-name &optional attrs &rest children) node
    (let* ((node-name (symbol-name node-name))
	   (tag-info (gethash node-name *tag-info*)))
      (if tag-info
	  (output-handled-lhtml-node node-name attrs children tag-info)
	  ;; else
	  (output-unhandled-lhtml-node node-name)))))

(defun output-lhtml (node)
  (cond
    ((stringp node)
     (<ah node))
    ((listp node)
     (output-lhtml-node node))
    (t
     (error "unhandled case ~a in output-lhtml" node))))
