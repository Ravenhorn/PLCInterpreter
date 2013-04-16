(define insert-class-method
  (lambda (func_env class_env)
    (cons (car class_env) (cons (cadr class_env) (cons func_env (cdddr class_env))))))

(define get-class-body
  (lambda (class)
    (cadddr class)))

(define get-class-name
  (lambda (class)
    (cadr class)))
     
(define get-parent-name
  (lambda (class)
    (cond
      ((null? (caddr class)) '())
      (else (cadr (caddr class))))))

(define get-class-var-method-names
  (lambda (class)
    (cons (caaar class) (caar (caddr class)))))

(define get-class-var-method-vals
  (lambda (class)
    (cons (cadaar class) (cadar (caddr class)))))
