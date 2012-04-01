#| ################################################################

Paul Graham's chapter 15 (see http://goo.gl/1OCc5) from Ansi COMMON
LISP (see http://www.paulgraham.com/acl.html) implements a minimal
Prolog system. It can't do everything that Prolog can do but, heh,
what do you expect of 100 lines of code.

################################################################ |#

(defun match (x y &optional binds)
  (cond 
   ((eql x y)        (values binds t))
   ((assoc x binds)  (match (binding x binds) y binds))
   ((assoc y binds)  (match x (binding y binds) binds))
   ((var? x)         (values (cons (cons x y) binds) t))
   ((var? y)         (values (cons (cons y x) binds) t))
   (t
    (when (and (consp x) (consp y))
      (multiple-value-bind (b2 yes) 
                           (match (car x) (car y) binds)
        (and yes (match (cdr x) (cdr y) b2)))))))

(defun var? (x)
  (and (symbolp x) 
       (eql (char (symbol-name x) 0) #\?)))

(defun binding (x binds)
  (let ((b (assoc x binds)))
    (if b
        (or (binding (cdr b) binds)
            (cdr b)))))

(defvar *rules* (make-hash-table))

(defmacro <- (con &optional ant)
  `(length (push (cons (cdr ',con) ',ant)
                 (gethash (car ',con) *rules*))))

(defun prove (expr &optional binds)
  (case (car expr)
    ; cs310 students! insert new code here
     (and          (prove-and (reverse (cdr expr)) binds))
     (or           (prove-or  (cdr expr) binds))
     (not          (prove-not (cadr expr) binds))
     (t            (prove-simple (car expr) (cdr expr) binds))
     (is           (prove-is binds))   
     (do           (prove-do binds))
     (say          (prove-say binds)))) 

(defun prove-code (expr binds)
  (labels ((lets (binds want)
	     (mapcar #'(lambda (x) `(,x ',(binding x binds))) want)))
    (let* ((vars (lets binds (vars-in expr)))
	   (code `(let ,vars ,expr)))
      (eval code))))

(defun prove-simple (pred args binds)
  (mapcan #'(lambda (r)
              (multiple-value-bind (b2 yes) 
                                   (match args (car r) 
                                          binds)
                (when yes
                  (if (cdr r) 
                      (prove (cdr r) b2) 
                      (list b2)))))
          (mapcar #'change-vars 
                  (gethash pred *rules*))))

(defun change-vars (r)
  (sublis (mapcar #'(lambda (v) (cons v (gensym "?")))
                  (vars-in r))
          r))

(defun vars-in (expr)
  (if (atom expr)
      (if (var? expr) (list expr))
      (union (vars-in (car expr))
             (vars-in (cdr expr)))))


(defun prove-and (clauses binds)
  (if (null clauses)
      (list binds)
      (mapcan #'(lambda (b)
                  (prove (car clauses) b))
              (prove-and (cdr clauses) binds))))

(defun prove-or (clauses binds)
  (mapcan #'(lambda (c) (prove c binds))
          clauses))

(defun prove-not (clause binds)
  (unless (prove clause binds)
    (list binds)))

(defun prove-is (binds))

(defun prove-do (binds))

(defun prove-say (binds))


(defmacro with-answer (query &body body)
  (let ((binds (gensym)))
    `(dolist (,binds (prove ',query))
       (let ,(mapcar #'(lambda (v)
                         `(,v (binding ',v ,binds)))
		     (vars-in query))
	 (declare (ignorable ,@(vars-in query)))
	 ,@body))))

(defun data0 ()
  (clrhash *rules*)
  (<- (parent donald nancy))
  (<- (parent donald debbie))
  (<- (male donald))
  (<- (father ?x ?y) (and (parent ?x ?y) (male ?x)))
  (<- (= ?x ?x))
  (<- (sibling ?x ?y) (and (parent ?z ?x)
			   (parent ?z ?y)
			   (not (= ?x ?y)))))

(defun data1 ()
  (clrhash *rules*) ; must start with this
  (<- (= ?x ?x))
  (<- (person matt  m  23  40000))
  (<- (person dean  m  90  90000))
  (<- (person clint m 100 100000000))
  (<- (person marge f  80 100000000))
  (<- (younger ?x ?y) (and (person ?x ?g1   ?age1 ?salary1)
			   (do (format t "~a~%" ?x))  ; <== needs "do"
			   (person ?y ?g2 ?age2)
			   (is ?factor (/ 11 10))
			   (not (= ?x ?y))
			   (< (* ?age1 ?factor) ?age2 ?salary2) ; <== needs "<"
			   (say "my isn't ~a too young" ?x))) ; <== needs say
  (<- (sameSex ?x ?y) (and (person ?x ?gender ?a1)
			   (person ?y ?gender ?a2)))
  (<- (hates   ?x ?y) (and (sameSex ?x ?y) (younger ?y ?x)))
)

(deftest !zero ()
  (data0)
  (test 
      "DONALD is the father of DEBBIE
       DONALD is the father of NANCY
       DEBBIE is the sibling of NANCY.
       NANCY is the sibling of DEBBIE.
       "
      (with-output-to-string (s)
	(with-answer (father ?x ?y)
	  (format s "~A is the father of ~A~%" ?x ?y))
	(with-answer (sibling ?x ?y)
	  (format s "~A is the sibling of ~A.~%" ?x ?y)))))
  
(deftest !one ()
  (data1)
  (with-output-to-string (s)
    (with-answer (hates ?x ?y)
      (format s "~a hates ~a~%" ?x ?y))))

(deftest !age ()
  (data-ages)
  (let ((x 0))
    (with-answer (age ?a)
      (incf x ?a))
    (format t "~a" x)
    (test 293 x)))
      
(defun data-ages ()
  (clrhash *rules*)
  (<- (person matt  m  23  40000))
  (<- (person dean  m  90  90000))
  (<- (person clint m 100 100000000))
  (<- (person marge f  80 100000000))
  (<- (age ?a) (and (person ?x ?gender ?a ?salary1))))