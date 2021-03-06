(define insert-class-method
  (lambda (func_env class_env)
    (cons (car class_env) (cons (cadr class_env) (cons func_env (cdddr class_env))))))

(define insert-inst-var
  (lambda (inst_env class_env)
    (cons (car class_env) (cons inst_env (cddr class_env)))))

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
    (append (caaar class) (caar (caddr class)))))

(define get-class-var-method-vals
  (lambda (class)
    (append (cadaar class) (cadar (caddr class)))))

(define get-instance-var-names
  (lambda (class)
    (caaadr class)))

(define faux-class
  (lambda (parent)
    (cons '((()())) (cons '((()())) (cons '((() ()))  (cons parent '()))))))

(define get-inst-enviro
  (lambda (c i)
    (cond
      ((null? c) '(()()))
      ((null? i) '(()()))
      (else (cons (get-instance-var-names c) (list (car i)))))))
