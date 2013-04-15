(define insert-class-method
  (lambda (func_env class_env)
    (cons (car class_env) (cons (cadr class_env) (cons func_env (cdddr class_env))))))

(define get-class-body
  (lambda (class)
    (cadddr class)))
     
(define get-parent-name
  (lambda (class)
    (cond
      ((null? (caddr class)) '())
      (else (cadr (caddr class))))))