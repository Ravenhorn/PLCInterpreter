(load "verySimpleParser.scm")

(define interpret
  (lambda (filename)
    (lookup 'return (interpret_sl (parser filename) ('((return)))))))

(define interpret_sl
  (lambda (ptree env)
    (cond
      ((null? ptree) env)
      (else (interpret_sl (cdr ptree) (interpret_stmnt (car ptree) env))))))

(define interpret-stmnt
  (lambda (stmnt env)
    (cond
      ((eq? '= (car statement)) (pret_assign stmnt env))
      ((eq? 'var (car statement)) (pret_declare stmnt env))
      ((eq? 'if (car statement)) (pret_if stmnt env))
      ((eq? 'return (car statement)) (pret_return stmnt env))
      (else (error "invalid operator")))))
     
(define pret_if
  (lambda (stmnt env)
    ()))

(define getBool
  (lambda (op)
    (cond
      ((eq? '>) >)
      ((eq? '==) =)
      ((eq? '<) <)
      ((eq? '!=) (lambda (n1 n2) (not (= n1 n2))))
      ((eq? '<=) <=)
      ((eq? '>=) >=)
      (else (error "invalid bool operator")))))

;TODO sideeffects
(define value
  (lambda (expr env)
    (cond
      ((number? expr) expr)
      ((not (pair? expr)) (lookup expr env))
      ((null? (cdr expr)) (value (car expr)))
     ; ((eq? '= (car expr)) 
      (else ((getOp (car expr)) (value (cadr expr)) (value (caddr expr)))))))
  
(define getOp
  (lambda (op)
    (cond
      ((eq? '+ op) +)
      ((eq? '- op) -)
      ((eq? '* op) *)
      ((eq? '/ op) quotient)
      ((eq? '% op) remainder)
      ((eq? '|| op) or)
      ((eq? '&& op) and)
      ((eq? '! op) not)
      (else (error "error in getOp, operator not found")))))

(define operator?
  (lambda (op)
    (cond
      ((eq? '+ op) #t)
      ((eq? '- op) #t)
      ((eq? '* op) #t)
      ((eq? '/ op) #t)
      ((eq? '% op) #t)
      ((eq? '|| op) #t)
      ((eq? '&& op) #t)
      ((eq? '! op) #t)
      (else #f))))

(define lookup
  (lambda (var env)
    (cond
      ((null? env) (error "variable not declared"))
      ((eq? var (caar env))
       (cond
         ((null? (cdar env)) (error "variable declared but not initialized"))
         (else (cadar env))))
      (else (lookup var (cdr env))))))

(define bind
  (lambda (var val env)
    (cond
      ((null?
      ((or (number? val) (boolean? val)) (cons (cons var (cons val '())) env))
      (else (error "invalid type, variables must be an integer or boolean")))))

(define pret_declare
  (lambda (stmnt env)
    (cond
      ((null? stmnt) (error "null arg passed to declare"))
      ((null? (cdr stmnt)) (bind (car stmnt) '() env))
      ((list? (cdr stmnt))
        (cond
          ((operator? (cadr stmnt))(pret_declare (cons (car stmnt) (cons (car (value (cdr stmnt) env)) '())) (cdr (value (cdr stmnt) env))))
          (else (pret_assign (cons (car stmnt) (cons (car (interpret-stmnt (cdr stmnt) env)) '())) (cdr (interpret-stmnt (cdr stmnt) env))))))
      (else (cons (car stmnt) (cons (cdr stmnt) '()))))))
      
