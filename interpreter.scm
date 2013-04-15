;KALAA Interpreter
;Stuart Long and Jason Kuster
;EECS 345 Interpreter 3
(load "classParser.scm")
(load "interpreter_class_helpers.scm")
(load "interpreter_environment.scm")
(load "interpreter_statements.scm")

(define interpret-class
  (lambda (filename mainclass)
    (call/cc (lambda (ret)
               (interpret-class-sl (parser filename) (new-env) (lambda (v) (interpret-sl (cadr (lookup 'main '() (lookup mainclass v '() '()) '())) v (lookup mainclass v '() '()) ret (lambda (env) (error "break called outside of a loop")) (lambda (env)(error "continue called outside of a loop")))))))))

(define interpret-class-sl
  (lambda (ptree env k) 
    (cond
      ((null? ptree) (k env))
      (else (interpret-class-sl (cdr ptree) (bind (cadar ptree) (interpret-class-body (get-class-body (car ptree)) (new-class-env (get-parent-name (car ptree)))) env) (lambda (v) (k v)))))))
  
;(define interpret
 ; (lambda (filename)
  ;  (call/cc (lambda (ret)
   ;            (interpret-global-sl (parser filename) (new-env) (lambda (v) (interpret-sl (cadr (lookup 'main v)) v ret (lambda (env) (error "break called outside of a loop")) (lambda (env)(error "continue called outside of a loop")))))))))

(define interpret-class-body
  (lambda (ptree env)
    (cond
      ((null? ptree) env)
      (else (interpret-class-body (cdr ptree) (interpret-class-stmnt (car ptree) env))))))

(define interpret-class-stmnt
  (lambda (stmnt env)
    (cond
      ((eq? 'static-var (car stmnt)) (cons (pret-declare stmnt (car env) '() '()) (cdr env))) ;TODO are we sure about passing null's to pret-declare?
      ((eq? 'static-function (car stmnt)) (insert-class-method (pret-func-def stmnt (caddr env) '() '()) env));ditto as above
      ;(else (error (car stmnt))))))
      (else (error "invalid global parse tree")))))