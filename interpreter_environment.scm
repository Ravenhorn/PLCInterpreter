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
      ((not (null? env)) (lookup-env var env (lambda (val)
                                               (cond
                                                 ((not (null? val)) val)
                                                 (else (lookup-ci var class instance (lambda (val)
                                                                                       (cond
                                                                                         ((null? val) (error "Variable not declared"))
                                                                                         (else val)))))))))
      (else (lookup-ci var class instance (lambda (val)
                                            (cond
                                              ((null? val) (error "Variable not declared"))
                                              (else val))))))))

(define lookup-env
  (lambda (var env k)
    (cond
      ((null? env) '())
      ((not (null? (k (lookvar var (caar env) (cadar env))))) (lookvar var (caar env) (cadar env)))
      (else (k (lookup var (cdr env)))))))

(define lookvar
  (lambda (var varlist vallist)
    (cond
      ((null? varlist) '())
      ((eq? var (car varlist))
       (cond
         ((null? (unbox (car vallist))) (error "variable/method declared but not initialized"))
         (else (unbox (car vallist)))))
      (else (lookvar var (cdr varlist) (cdr vallist))))))

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
      (else (k (lookvar var (reverse (get-class-var-method-names class)) (reverse (get-class-var-method-vals class)))))))) ;stuart's fix
      ;(else (k (lookvar var (reverse (car class)) (reverse (cadr class)))))))) 

(define lookup-instance
  (lambda (var class instance k)
    (cond
      ((null? instance) (k '()))
      (else (k (lookvar var (reverse (get-class-var-method-names class)) (reverse (car instance)))))))))
      ;(else (k (lookvar var (reverse (car class)) (reverse (car instance)))))))) ;<-- same problem as lookup-class

(define bind
  (lambda (var val env)
    (cons (cons (cons var (caar env)) (cons (cons (box val) (cadar env)) '())) (cdr env))))

(define bind-deep
  (lambda (var val env)
    (cond
      ((null? env) (error "null environment"));shouldn't this error out?
      ((declared? var (cons (car env) '())) (handle-box var val (car env) (lambda (val enviro) env)))
      (else (cons (car env) (bind-deep var val (cdr env)))))))

(define handle-box
  (lambda (var val env k)
    (k (set-box! (get-box var (car env) (cadr env)) val) env)))

(define get-box
  (lambda (var vars vals)
    (cond
      ((null? vars) (error "couldn't find box"))
      ((eq? var (car vars)) (car vals))
      (else (get-box var (cdr vars) (cdr vals))))))

(define get-box-for-ref
  (lambda (var env)
    (cond
      ((null? env) (error "var not declared"))
      ((or (pair? var) (boolean? var) (number? var)) (error "only variables can be passed by reference"))
      ((declared? var (cons (car env) '())) (get-box var (caar env) (cadar env)))
      (else (get-box-for-ref var (cdr env))))))

(define bind-box
  (lambda (var box_val env)
    (cons (cons (cons var (caar env)) (cons (cons box_val (cadar env)) '())) (cdr env))))

(define declared?
  (lambda (var env)
    (cond
      ((null? env) #f)
      ((null? var) (error "null var"))
      ((eq? (car env) var) #t)
      ((and (list? (car env)) (declared? var (car env))) #t)
      (else (declared? var (cdr env))))))

(define get-func-env
  (lambda (env)
    (cond
      ((null? (cdr env)) env)
      (else (get-func-env (cdr env))))))

(define get-func-class
  (lambda (name env)
    (lookup name env '() '())))

(define new-class-env
  (lambda (parent)
    (cons (new-env) (cons (new-env) (cons (new-env) (cons parent '()))))))