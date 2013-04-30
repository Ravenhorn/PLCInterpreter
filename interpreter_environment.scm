(load "interpreter_class_helpers.scm")

(define new-env
  (lambda ()
    '((() ())) ))

(define push-frame
  (lambda (env)
    (cons (cons '() (cons '() '())) env)))

(define pop-frame
  (lambda (env)
    (cdr env)))

(define lookup
  (lambda (var env class instance)
    (cond
      ((not (null? env))  (lookup-env var env (lambda (val)
                                               (cond
                                                 ((not (null? val)) val)
                                                 (else (lookup-ci var class instance (lambda (val)
                                                                                       (cond
                                                                                         ((null? val) (begin (display "error on: ") (display var) (newline) (error "variable not declared")))
                                                                                         (else val)))))))))
      (else (lookup-ci var class instance (lambda (val)
                                            (cond
                                              ((null? val) (begin (display "error on: ") (display var) (newline) (error "variable not declared")))
                                              (else val))))))))

(define lookup-env
  (lambda (var env k)
    (cond
      ((null? env) '())
      ((not (null? (lookvar var (caar env) (cadar env) (lambda (v) v)))) (lookvar var (caar env) (cadar env) (lambda (v) (k v)))) ;turn this into a continuation
      (else (lookup-env var (cdr env) (lambda (v) (k v)))))))

(define lookvar
  (lambda (var varlist vallist k)
    (cond
      ((null? varlist) (k '()))
      ((eq? var (car varlist))
       (cond
         ((null? (unbox (car vallist))) (begin (display "error on: ") (display var) (newline) (error "variable not initialized")))
         (else (k (unbox (car vallist))))))
      (else (k (lookvar var (cdr varlist) (cdr vallist) (lambda (v) v)))))))

(define lookup-ci
  (lambda (var class instance k)
    (lookup-instance var class instance
                       (lambda (v)
                         (cond
                           ((null? v) (lookup-class var class (lambda (v) (k v))))
                           (else (k v)))))))

(define lookup-class
  (lambda (var class k)
    (cond
      ((null? class) (k '()))
      (else (k (lookvar var 
                        (reverse (get-class-var-method-names class))
                        (reverse (get-class-var-method-vals class))
                        (lambda (v)
                          (cond
                            ((null? v) (lookup-class var (cadddr class) (lambda (v) v)))
                            (else v)))))))))
      ;(else (k (lookvar var (reverse (car class)) (reverse (cadr class)))))))) 

(define lookup-instance
  (lambda (var class instance k)
    (cond
      ((null? instance) (k '()))
      (else (k (lookvar var (reverse (get-class-var-method-names class)) (reverse (car instance)) (lambda (v) v))))))) ;will need to be changed when objects implemented
      ;(else (k (lookvar var (reverse (car class)) (reverse (car instance)))))))) ;<-- same problem as lookup-class

(define bind
  (lambda (var val env)
    (cons (cons (cons var (caar env)) (cons (cons (box val) (cadar env)) '())) (cdr env))))

;(define bind-iv
;#  (lambda (var val class)
;#    (cond
;      ((null? class) (error "null class"))
;      (else (cons (car class) (cons (bind-deep var val (cadr class)) (cddr class)))))))

(define bind-deep
  (lambda (var val env)
    (cond
      ((null? env) (begin (display "error on: ") (display var) (display val) (newline) (error "null environment")));shouldn't this error out?
      ((declared? var (cons (car env) '()) '()) (handle-box var val (car env) (lambda (val enviro) env)))
      (else (cons (car env) (bind-deep var val (cdr env)))))))

(define handle-box
  (lambda (var val env k)
    (k (set-box! (get-box var (car env) (cadr env)) val) env)))

(define get-box
  (lambda (var vars vals)
    (cond
      ((null? vars) (begin (display "error on: ") (display var) (newline) (error "couldn't find box")))
      ((eq? var (car vars)) (car vals))
      (else (get-box var (cdr vars) (cdr vals))))))

(define get-box-for-ref
  (lambda (var env)
    (cond
      ((null? env) (begin (display "error on: ") (display var) (newline) (error "variable not declared")))
      ((or (pair? var) (boolean? var) (number? var)) (begin (display "error on: ") (display var) (newline) (error "only variables can be passed by reference")))
      ((declared? var (cons (car env) '()) '()) (get-box var (caar env) (cadar env)))
      (else (get-box-for-ref var (cdr env))))))

(define bind-box
  (lambda (var box_val env)
    (cons (cons (cons var (caar env)) (cons (cons box_val (cadar env)) '())) (cdr env))))

(define declared?
  (lambda (var env class)
    (declared-env? var env (lambda (v)
                             (if v
                                 #t
                                 (declared-class? var class (lambda (v) v)))))))

(define declared-env?
  (lambda (var env k)
    (cond
      ((null? env) (k #f))
      ((null? var) (error "null var"))
      ((eq? (car env) var) (k #t))
      ((and (list? (car env)) (declared-env? var (car env) (lambda (v) v))) (k #t))
      (else (declared-env? var (cdr env) (lambda (v) (k v)))))))

(define declared-class?
  (lambda (var class k)
    (cond
      ((null? class) (k #f))
      ((null? var) (error "null var"))
      (else (k (declared-env? var (car class) (lambda (v) v)))))))

(define declared-inst?
  (lambda (var class inst k)
    (cond
      ((null? class) (k #f))
      ((null? var)))))

(define get-func-env
  (lambda (env)
    (cond
      ((null? (cdr env)) env)
      (else (get-func-env (cdr env))))))

(define get-func-class
  (lambda (name env)
    (lookup name env '() '())))

(define new-class-env
  (lambda (parent name)
    (cons (new-env) (cons (new-env) (cons (push-frame (make-def-const (new-env) name)) (cons parent '()))))))

(define lookup-method
  (lambda (name class numb_args)
    (letrec ((loop (lambda (var_l val_l)
                     (lookvar name var_l val_l (lambda (v) (cond
                                                             ((eq? (length (car v)) numb_args) v)
                                                              (else (loop (name (cdr var_l) (cdr val_l))))))))))
      (loop (caar (caddr class)) (cadar (caddr class))))))

(define make-def-const
  (lambda (env name)
    (bind name '(() ((funcall super))) env)))

(define add-exception-val
  (lambda (val env)
    (bind 'e val env)))
