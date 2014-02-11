(in-package #:pjs-yaclml)


(defun test-merge-progns ()
  (macrolet ((tc (name code expected)
	       `(let ((result (merge-progns ',code)))
		  (unless (equal ',expected result)
		    (format t "test ~a failed, expected:~% ~w~% but got~% ~w~%"
			    ',name ',expected result)))))
    (tc symbols foo foo)
    (tc strings "foo" "foo")
    (tc non-progn 
	(defun foo () foo)
	(defun foo () foo))
    (tc simple-progn
	(progn 1 2 3)
	(progn 1 2 3))
    (tc double-progn
	(progn 1 (progn 2) 3)
	(progn 1 2 3))
    (tc test-failure
	(progn (progn (<ai "<html") (<ai ">")) (progn (<ai "<head") (<ai ">")) (progn (<ai "<title") (<ai ">")) (<ai "hello") (<ai "</title>") (<ai "</head>") (<ai "<body") (<ai ">") (<ai "<p") (<ai ">") name (<ai "</p>") (<ai "<p") (<ai ">") (<:as-html name) (<ai "</p>") (<ai "</body>") (<ai "</html>"))
	nil)))

(defun test-merge-ai ()
  (macrolet ((tc (name code expected)
	       `(let ((result (merge-ai ',code)))
		  (unless (equal ',expected result)
		    (format t "test ~w failed, expected:~%~w~%but got~%~W~%"
			    ',name ',expected result)))))
    (tc nil (nil) ())
    (tc symbols (foo) (foo))
    (tc strings ("foo") ("foo"))
    (tc single ((<ai "foo")) ((<ai "foo")))
    (tc double ((<ai "foo") (<ai "bar")) ((<ai "foobar")))
    (tc separated ((<ai "foo") foobar (<ai "bar")) ((<ai "foo") foobar (<ai "bar")))
    (tc example-failure
	(progn (<ai "<html") (<ai ">") (<ai "<head") (<ai ">") (<ai "<title") (<ai ">") (<ai "hello") (<ai "</title>") (<ai "</head>") (<ai "<body") (<ai ">") (<ai "<p") (<ai ">") name (<ai "</p>") (<ai "<p") (<ai ">") (<:as-html name) (<ai "</p>") (<ai "</body>") (<ai "</html>"))
	(progn (<ai "<html><head><title>hello</title></head><body><p>") name (<ai "</p><p>") (<:as-html name) (<ai "</p></body></html>")))))