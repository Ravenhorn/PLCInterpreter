(load "verySimpleParser.scm")

(define interpret
  (lambda (filename)
    (lookup 'return (interpret_sl (parser filename) '((return))))))

(define interpret_sl
  (lambda (ptree env)
    (cond
      ((null? ptree) env)
      (else (interpret_sl (cdr ptree) (interpret_stmnt (car ptree) env))))))

(define interpret_stmnt
  (lambda (stmnt env)
    (cond
      ;((null? stmnt) env)
      ;((not (pair? stmnt)) env)
      ((pair? (car stmnt)) (interpret_stmnt (car stmnt) env))
      ((eq? '= (car stmnt)) (cadr (pret_assign stmnt env)))
      ((eq? 'var (car stmnt)) (pret_declare stmnt env))
      ((eq? 'if (car stmnt)) (pret_if stmnt env))
      ((eq? 'return (car stmnt)) (pret_return stmnt env))
     ; ((operator? (car stmnt)) (cadr (value stmnt env)))
      (else (error stmnt)))))

(define pret_return
  (lambda (stmnt env)
    (bind 'return (car (value (cadr stmnt) env)) (cadr (value (cadr stmnt) env)))))
     
(define pret_if
  (lambda (stmnt env)
    (cond
      ((null? (cdddr stmnt)) ;no else
       (cond
         ((car (eval_if (cadr stmnt) env)) (interpret_sl (cons (caddr stmnt) '()) (cadr (eval_if (cadr stmnt) env))))
         (else (cadr (eval_if (cadr stmnt) env)))))
      (else ;has an else
       (cond
         ((car (eval_if (cadr stmnt) env)) (interpret_sl (cons (caddr stmnt) '()) (cadr (eval_if (cadr stmnt) env))))
        ; ((car (eval_if (cadr stmnt) env)) (error stmnt))
         (else (interpret_sl (cons (cdddr stmnt) '()) (cadr (eval_if (cadr stmnt) env)))))))))

(define eval_if
  (lambda (if env)
    (cond
      ((null? (cddr if)) (cons ((getBool (car if)) (car (value (cadr if) env))) (cons (cadr (value (cadr if) env)) '())))
      (else (cons ((getBool (car if)) (car (value (cadr if) env)) (car (value (caddr if) (cadr (value (cadr if) env))))) 
                  (cons (cadr (value (caddr if) (cadr (value (cadr if) env)))) '()))))))
                                      
(define getBool
  (lambda (op)
    (cond
      ((eq? '> op) >)
      ((eq? '== op) =)
      ((eq? '< op) <)
      ((eq? '!= op) (lambda (n1 n2) (not (= n1 n2))))
      ((eq? '<= op) <=)
      ((eq? '>= op) >=)
      ((eq? '|| op) (lambda (b1 b2) (or b1 b2)));for some reason just returning or gives a syntax error
      ((eq? '&& op) (lambda (b1 b2) (and b1 b2)));for some reason just returning or gives a syntax error
      ((eq? '! op) not)
      (else (error "invalid bool operator")))))

(define value
  (lambda (expr env)
    (cond
      ((or (number? expr) (boolean? expr)) (cons expr (cons env '())))
      ((not (pair? expr)) (cons (lookup expr env) (cons env '())))
      ((null? (cdr expr)) (value (car expr) env))
      ((eq? '= (car expr)) (pret_assign expr env))
      (else (cons ((getOp (car expr)) (car (value (cadr expr) env)) (car (value (caddr expr) (cadr (value (cadr expr) env))))) 
                  (cons (cadr (value (caddr expr) (cadr (value (cadr expr) env)))) '()))))))

(define getOp
  (lambda (op)
    (cond
      ((eq? '+ op) +)
      ((eq? '- op) -)
      ((eq? '* op) *)
      ((eq? '/ op) quotient)
      ((eq? '% op) remainder)
      ((eq? '|| op) (lambda (b1 b2) (or b1 b2)));for some reason just returning or gives a syntax error
      ((eq? '&& op) (lambda (b1 b2) (and b1 b2)));for some reason just returning or gives a syntax error
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
      ((null? val) (cons (cons var '()) env))
      ((or (number? val) (boolean? val)) (cons (cons var (cons val '())) env))
      ;(else (error "invalid type, variables must be an integer or boolean")))))
(else (error val)))))

(define declared?
  (lambda (var env)
    (cond
      ((null? env) #f)
      ((null? var) (error "null var"))
      ((eq? (caar env) var) #t)
      (else (declared? var (cdr env))))))

(define pret_declare
  (lambda (stmnt env)
    (cond
      ((null? stmnt) (error "null arg passed to declare"))
      ((null? (cddr stmnt)) (bind (cadr stmnt) '() env))
      (else (bind (cadr stmnt) (car (value (cddr stmnt) env)) (cadr (value (caddr stmnt) env)))))))

(define pret_assign
  (lambda(stmnt env)
    (cond
      ((null? stmnt) (error "null arg passed to assign"))
      ((null? (cddr stmnt)) (error "no value to assign"))
      ((declared? (cadr stmnt) env) (cons (car (value (caddr stmnt) env)) (cons (bind (cadr stmnt) (car (value (caddr stmnt) env)) (cadr (value (caddr stmnt) env))) '())))
      (else (error "unrecognized lhs")))))

