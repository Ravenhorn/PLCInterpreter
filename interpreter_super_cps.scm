;KALAA Interpreter
;Stuart Long and Jason Kuster
;EECS 345 Interpreter 2

(load "loopSimpleParser.scm")

(define interpret
  (lambda (filename)
    (call/cc (lambda (ret)
               (interpret-sl (parser filename) (new-env) ret (lambda (env) (error("break called outside of a loop"))) (lambda (env)(error("continue called outside of a loop"))))))))

(define interpret-sl
  (lambda (ptree env ret brk cont)
    (cond
      ((null? ptree) env)
      (else (interpret-sl (cdr ptree) (interpret-stmnt (car ptree) env ret brk cont) ret brk cont)))))

(define interpret-stmnt
  (lambda (stmnt env ret brk cont)
    (cond
      ((pair? (car stmnt)) (interpret-stmnt (car stmnt) env))
      ((eq? '= (car stmnt)) (pret-assign stmnt env (lambda (val env) env)))
      ((eq? 'var (car stmnt)) (pret-declare stmnt env))
      ((eq? 'if (car stmnt)) (pret-if stmnt env ret brk cont))
      ((eq? 'return (car stmnt)) (ret (pret-return stmnt env)))
      ((eq? 'while (car stmnt)) (pret-while stmnt env ret))
      ((eq? 'break (car stmnt)) (brk env))
      ((eq? 'continue (car stmnt)) (cont env))
      ((eq? 'begin (car stmnt)) (interpret-sl (cdr stmnt) env ret brk cont))
      (else (error "invalid parse tree")))))

(define pret-while
  (lambda (stmnt enviro return)
    (call/cc (lambda (break)
               (letrec ((loop (lambda (cond body env)
                                (eval-if cond env (lambda (if1 if_enviro)
                                                    (if  if1
                                                        (loop cond body (interpret-stmnt body if_enviro return break (lambda (e) (loop cond body e))))
                                                        env))))))
                        (pop-frame (loop (cadr stmnt) (caddr stmnt) (push-frame enviro))))))))

(define pret-return
  (lambda (stmnt env)
    (value (cadr stmnt) env (lambda (val enviro) val))))

(define pret-declare
  (lambda (stmnt env)
    (cond
      ((null? stmnt) (error "null arg passed to declare"))
      ((null? (cddr stmnt)) (bind (cadr stmnt) '() env))
      (else (bind (cadr stmnt) (value (cddr stmnt) env (lambda (val enviro) val)) (value (caddr stmnt) env (lambda (val2 enviro2) enviro2)))))))

(define pret-assign
  (lambda(stmnt env k)
    (cond
      ((null? stmnt) (error "null arg passed to assign"))
      ((null? (cddr stmnt)) (error "no value to assign"))
      ((declared? (cadr stmnt) env) (value (caddr stmnt) env (lambda (val enviro) (k val (bind-deep (cadr stmnt) val enviro))))))))

(define pret-if
  (lambda (stmnt env ret brk cont)
    (eval-if (cadr stmnt) env 
             (lambda (if1 enviro)
               (cond
                 ((null? (cdddr stmnt)) ;no else
                  (cond
                    (if1 (pop-frame (interpret-stmnt (caddr stmnt) (push-frame enviro) ret brk cont)))
                    (else enviro)))
                 (else ;has an else
                  (cond
                    (if1 (pop-frame (interpret-stmnt (caddr stmnt) (push-frame enviro) ret brk cont)))
                    (else (pop-frame (interpret-stmnt (cadddr stmnt) (push-frame enviro) ret brk cont))))))))))

(define eval-if
  (lambda (if env k)
    (value (cadr if) env
           (lambda (val enviro)
             (cond
               ((null? (cddr if)) (k ((getBool (car if)) val) enviro))
               (else (value (caddr if) enviro (lambda (val2 enviro2) (k ((getBool (car if)) val val2) enviro2)))))))))
                                      
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
      (else (error "invalid operator")))))

(define value
  (lambda (expr env k)
    (cond
      ((or (number? expr) (boolean? expr)) (k expr env))
      ((not (pair? expr)) (k (lookup expr env) env))
      ((null? (cdr expr)) (value (car expr) env (lambda (vals enviro) (k vals enviro))))
      ((eq? '= (car expr)) (pret-assign expr env (lambda (vals enviro) (k vals enviro))))
      ((and (eq? '- (car expr)) (null? (cddr expr))) (value (cdr expr) env (lambda (vals enviro) (k (* -1 vals) enviro))))
      (else (value (cadr expr) env (lambda (val enviro) (value (caddr expr) enviro 
                                             (lambda (val2 enviro2) (k ((getOp (car expr)) val val2) enviro2)))))))))

(define getOp
  (lambda (op)
    (cond
      ((eq? '+ op) +)
      ((eq? '- op) -)
      ((eq? '* op) *)
      ((eq? '/ op) quotient)
      ((eq? '% op) remainder)
      (else (getBool op)))))

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
  (lambda (var env)
    (cond
      ((null? env) (error "variable not declared"))
      ((not (null? (lookvar var (caar env) (cadar env)))) (lookvar var (caar env) (cadar env)))
      (else (lookup var (cdr env))))))

(define lookvar
  (lambda (var varlist vallist)
    (cond
      ((null? varlist) '())
      ((eq? var (car varlist))
       (cond
         ((null? (car vallist)) (error "variable declared but not initialized"))
         (else (unbox (car vallist)))))
      (else (lookvar var (cdr varlist) (cdr vallist))))))

(define bind
  (lambda (var val env)
    (cond
      ((or (or (number? val) (boolean? val)) (null? val)) (cons (cons (cons var (caar env)) (cons (cons (box val) (cadar env)) '())) (cdr env)))
      (else (error "invalid type, variables must be an integer or boolean")))))

(define bind-deep
  (lambda (var val env)
    (cond
      ((null? env) '())
      ((declared? var (cons (car env) '())) (bind var val env))
      (else (cons (car env) (bind-deep var val (cdr env)))))))

(define declared?
  (lambda (var env)
    (cond
      ((null? env) #f)
      ((null? var) (error "null var"))
      ((eq? (car env) var) #t)
      ((and (list? (car env)) (declared? var (car env))) #t)
      (else (declared? var (cdr env))))))
