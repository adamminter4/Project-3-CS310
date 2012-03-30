(handler-bind ((style-warning #'muffle-warning)) 
  (mapc 'load '(
		"../tricks.lisp"
		"15.lisp"
		)))

(defun ! () (load "main.lisp"))

(defun main () 
  (tests))


(defun hello (&optional (who "world"))
	(format nil "hello ~a~%" who))
