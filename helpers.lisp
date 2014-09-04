(in-package #:pjs-yaclml)

(defun compile-time-constant (form)
  (or (eq form t)
      (eq form nil)
      (stringp form)
      (characterp form)
      (numberp form)))

;; (/= (h-t-c *entity-char-map*)
;;     (+ (h-t-c *char-entity-map*)
;;        (h-t-c *double-char-entity-map*)))  
(defun load-entities ()
  (setf *double-char-entity-map* (make-hash-table :test 'equal))
  (setf *char-entity-map* (make-hash-table :test 'equal))
  (setf *entity-char-map* (make-hash-table :test 'equal))
  (with-open-file (s (asdf:system-relative-pathname '#:pjs-yaclml "entities.sexp"))
    (let ((ones (read s))
	  (twos (read s)))
      (while ones
	(let* ((name (pop ones))
	       (char (pop ones))
	       (char (code-char char))
	       (chars (list char)))
	  (setf (gethash chars *char-entity-map*)
		(sconc "&" name ";"))
	  (setf (gethash name *entity-char-map*)
		chars)))
      (while twos
	(let* ((name (pop twos))
	       (char1 (pop twos))
	       (char1 (code-char char1))
	       (char2 (pop twos))
	       (char2 (code-char char2))
	       (chars (list char1 char2)))
	  (setf (gethash chars *double-char-entity-map*)
		(sconc "&" name ";"))
	  (setf (gethash name *entity-char-map*)
		chars))))))

(eval-when (:load-toplevel :execute)
  (load-entities))

(defmacro with-no-compiler-notes (&body body)
  #+sbcl
  `(locally (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
     ,@body)
  #-sbcl
  `(progn ,@body))

(defun escape-as-html (str)
  (declare (optimize (speed 3)
		     (safety 3)))
  (cond
    ((null str)
     nil)
    ((not (stringp str))
     (escape-as-html (princ-to-string str)))
    (t
     (with-output-to-string (s)
       ;; can't use DOVECTOR because there are some double char entities, so we 
       ;; need to incf count var in loop, and DOTIMES is allowed to bind the var
       ;; rather than mutate
       (let* ((input-length (length str))
	      (input-length-1 (1- input-length))
	      (safe-char-list +safe-char-list+))
	 (declare (type (mod #.array-dimension-limit) input-length input-length-1)
		  (type (simple-array character) safe-char-list))
	 (do ((i 0 (incf i)))
	     ((= i input-length))
	   (declare (type (mod #.array-dimension-limit) i))
	   (let* ((ch (with-no-compiler-notes
			(char str i)))
		  (double-ch (when (< i input-length-1)
			       (list ch
				     (with-no-compiler-notes
				       (char str (1+ i)))))))
	     (cond
	       ((find ch safe-char-list)
		(write-char ch s))
	       ((and double-ch
		     (awhen (gethash double-ch *double-char-entity-map*)
		       (write-sequence it s)))
		(incf i))
	       ((awhen (gethash (list ch) *char-entity-map*)
		  (write-sequence it s))
		t)
	       (t
		(write-char ch s))))))))))

(defun merge-progns (form)
  (flet ((progn-form-p (form)
	   (and (listp form)
		(eq (first form)
		    'progn))))
    (if (progn-form-p form)
	(dolist-c (el form)
	  (if (progn-form-p el)
	      (dolist (el (rest (merge-progns el)))
		(collect el))
	      ;; else
	      (collect el)))
	;; else
	form)))

(defun merge-ai (root)
  (let ((root (remove-if #'null root))
	current-constant)
    (with-collector (collect)
      (labels ((constant-ai (form)
		 (and (listp form)
		      (eq (first form)
			  '<ai)
		      (or (stringp (second form))
			  (characterp (second form))
			  (numberp (second form)))))
	       (emit-constant ()
		 (when current-constant
		   (collect `(<ai ,current-constant))
		   (setf current-constant nil))))
	(dolist (form root)
	  (if (constant-ai form)
	      (let* ((arg (second form))
		     (arg (if (stringp arg)
			      arg
			      ;; else
			      (princ-to-string arg))))
		(setf current-constant
		      (sconc current-constant arg)))
	      ;; else
	      (progn
		(emit-constant)
		(collect form))))
	(emit-constant)))))

(defun custom-attr-p (form)
  (and (listp form)
       (eq (first form)
	   'custom-attr)))

