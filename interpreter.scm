;KALAA Interpreter
;Stuart Long and Jason Kuster
;EECS 345 Interpreter 5
(load "classParser.scm")
(load "interpreter_class_helpers.scm")
(load "interpreter_environment.scm")
(load "interpreter_statements.scm")

(define interpret
  (lambda (filename mainclass)
    (call/cc (lambda (ret)
               (interpret-class-sl (parser filename) (new-env) (lambda (v) (interpret-sl (cadr (lookup 'main '() (lookup mainclass v '() '()) '())) v (lookup mainclass v '() '()) '() ret (lambda (env) (error "break called outside of a loop")) (lambda (env)(error "continue called outside of a loop")) (lambda (error) (error "throw called outside try")))))))))

(define interpret-class-sl
  (lambda (ptree env k) 
    (cond
      ((null? ptree) (k env))
      (else (interpret-class-sl (cdr ptree) (bind (cadar ptree) (interpret-class-body (get-class-body (car ptree)) (new-class-env (begin 
                                                                                                                                    (cond
                                                                                                                                      ((null? (get-parent-name (car ptree))) '())
                                                                                                                                      (else (lookup (get-parent-name (car ptree)) env '() '())))) (get-class-name (car ptree)))                                                                                      
                                                                                                                                  (get-class-name (car ptree))) env) (lambda (v) (k v)))))))

;(define interpret
 ; (lambda (filename)
  ;  (call/cc (lambda (ret)
   ;            (interpret-global-sl (parser filename) (new-env) (lambda (v) (interpret-sl (cadr (lookup 'main v)) v ret (lambda (env) (error "break called outside of a loop")) (lambda (env)(error "continue called outside of a loop")))))))))

(define interpret-class-body
  (lambda (ptree env name)
    (cond
      ((null? ptree) env)
      (else (interpret-class-body (cdr ptree) (interpret-class-stmnt (car ptree) env name) name)))))

(define interpret-class-stmnt
  (lambda (stmnt env name)
    ;(begin (display 'HEY) (display stmnt) (newline) (display env) (newline)
    (cond
      ((eq? 'static-var (car stmnt)) (cons (pret-declare stmnt (car env) (faux-class (cadddr env)) '()) (cdr env))) ;TODO are we sure about passing nulls to pret-declare?
      ((eq? 'static-function (car stmnt)) ;A static function?
       (cond
         ((eq? (cadr stmnt) name) (insert-class-method (cons (caaddr env) (pret-func-def stmnt (cdaddr env) name)) env)) ;The constructor?
         (else (insert-class-method (pret-func-def stmnt (caddr env) name) env)))) ;Something else
      ((eq? 'var (car stmnt)) (insert-inst-var (bind (cadr stmnt) (caddr stmnt) (cadr env)) env))
      ((eq? 'function (car stmnt)) (insert-class-method (pret-func-def stmnt (caddr env) name) env))
      (else (error "invalid global parse tree")))))