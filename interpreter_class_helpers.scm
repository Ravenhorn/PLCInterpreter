(define insert-class-method
  (lambda (func_env class_env)
    (cons (car class_env) (cons (cadr class_env) (cons func_env (cdddr class_env))))))

(define pret-func-def
  (lambda (stmnt env)
    (bind (cadr stmnt)
          (cons (caddr stmnt) (cons (cadddr stmnt) (cons (lambda (v) (get-func-env v)) ;<--handle recursion
                                                         '()))) env)))

(define get-class-body
  (lambda (class)
    (cadddr class)))
     
(define get-parent-name
  (lambda (class)
    (cond
      ((null? (caddr class)) '())
      (else (cadr (caddr class))))))