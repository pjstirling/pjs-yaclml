(in-package #:pjs-yaclml)

(defun compile-time-constant (form)
  (or (eq form t)
      (eq form nil)
      (stringp form)
      (characterp form)
      (numberp form)))

(declaim (inline <ai))
(defun <ai (arg)
  (princ arg *yaclml-stream*))

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
		     (debug 0)))
  (macrolet ((incf* (place &optional val)
	       `(with-no-compiler-notes
		  ,(if val
		       `(incf ,place ,val)
		       ;; else
		       `(incf ,place)))))
    (if (null str)
	nil
	;; else
	(if (not (stringp str))
	    (escape-as-html (princ-to-string str))
	    ;; else
	    (locally
		(declare (type simple-string str))
	      (let* ((input-length (length str))
		     (input-length-1 (1- input-length))
		     (length 0)
		     (buffer (make-array input-length :fill-pointer 0))) 
		(do ((i 0 (incf i)))
		    ((= i input-length))
		  (declare (type fixnum length i)
			   (type simple-vector +safe-char-list+))
		  (let ((char (char str i)))
		    (if (find char +safe-char-list+)
			(progn
			  (vector-push-extend char buffer)
			  (incf* length))
			;; else
			(if (< i input-length-1)
			    (aif (gethash (list char
						(char str (1+ i)))
					  *double-char-entity-map*)
				 (locally (declare (type simple-string it))
				   (vector-push it buffer)
				   (incf* length (length it)))
				 ;; else
				 (aif (gethash (list char)
					       *char-entity-map*)
				      (locally (declare (type simple-string it))
					(vector-push it buffer)
					(incf* length (length it)))
				      ;; else
				      (progn
					(vector-push char buffer)
					(incf* length))))
			    ;; else
			    (aif (gethash (list char)
					  *char-entity-map*)
				 (locally (declare (type simple-string it))
				   (vector-push it buffer)
				   (incf* length (length it)))
				 ;; else
				 (progn
				   (vector-push char buffer)
				   (incf* length)))))))
		(let ((result (make-string length))
		      (i 0))
		  (dovector (el buffer)
		    (declare (type (or character simple-string) el))
		    (if (characterp el)
			(progn
			  (setf (char result i) el)
			  (incf i))
			;; else
			(dovector (ch el)
			  (setf (char result i) ch)
			  (incf i))))
		  result)))))))

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

