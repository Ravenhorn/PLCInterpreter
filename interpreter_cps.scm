;KALAA Interpreter
;Stuart Long and Jason Kuster
;EECS 345 Interpreter 2

(load "loopSimpleParser.scm")

(define interpret
  (lambda (filename)
    (call/cc (lambda (ret)
               (interpret-sl (parser filename) '((() ())) ret (lambda (env) (error("break called outside of a loop"))) (lambda (env)(error("continue called outside of a loop"))))))))

(define interpret-sl
  (lambda (ptree env ret brk cont)
    (cond
      ((null? ptree) env)
      (else (interpret-sl (cdr ptree) (interpret-stmnt (car ptree) env ret brk cont) ret brk cont)))))

(define interpret-stmnt
  (lambda (stmnt env ret brk cont)
    (cond
      ((pair? (car stmnt)) (interpret-stmnt (car stmnt) env))
      ((eq? '= (car stmnt)) (cadr (pret-assign stmnt env)))
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
                                (eval-if cond env (lambda (if_env)
                                                    (if (car if_env)
                                                        (loop cond body (interpret-stmnt body (cadr if_env) return break (lambda (e) (loop cond body e))))
                                                        env))))))
                        (pop-frame (loop (cadr stmnt) (caddr stmnt) (push-frame enviro))))))))

(define pret-return
  (lambda (stmnt env)
    (car (value (cadr stmnt) env (lambda (v) v)))))

(define pret-declare
  (lambda (stmnt env)
    (cond
      ((null? stmnt) (error "null arg passed to declare"))
      ((null? (cddr stmnt)) (bind (cadr stmnt) '() env))
      (else (bind (cadr stmnt) (car (value (cddr stmnt) env (lambda (v) v))) (cadr (value (caddr stmnt) env (lambda (v) v))))))))

(define pret-assign
  (lambda(stmnt env)
    (cond
      ((null? stmnt) (error "null arg passed to assign"))
      ((null? (cddr stmnt)) (error "no value to assign"))
      ((declared? (cadr stmnt) env) (value (caddr stmnt) env 
                                           (lambda (val_caddr) (cons (car val_caddr) (cons (bind-deep (cadr stmnt) (car val_caddr) (cadr val_caddr)) '()))))))))

(define pret-if
  (lambda (stmnt env ret brk cont)
    (eval-if (cadr stmnt) env 
             (lambda (if_env)
               (cond
                 ((null? (cdddr stmnt)) ;no else
                  (cond
                    ((car if_env) (pop-frame (interpret-stmnt (caddr stmnt) (push-frame (cadr if_env)) ret brk cont)))
                    (else (cadr if_env))))
                 (else ;has an else
                  (cond
                    ((car if_env) (pop-frame (interpret-stmnt (caddr stmnt) (push-frame (cadr if_env)) ret brk cont)))
                    (else (pop-frame (interpret-stmnt (cadddr stmnt) (push-frame (cadr if_env)) ret brk cont))))))))))

(define eval-if
  (lambda (if env k)
    (value (cadr if) env
           (lambda (val_cadr)
             (cond
               ((null? (cddr if)) (k (cons ((getBool (car if)) (car val_cadr)) (cons (cadr val_cadr) '()))))
               (else (k (value (caddr if) (cadr val_cadr) (lambda (val_caddr) 
                                                            (cons ((getBool (car if)) (car val_cadr) (car val_caddr))
                                                                  (cons (cadr val_caddr) '())))))))))))
                                      
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
      (else (error op)))))
      ;(else (error "invalid bool operator")))))

(define value
  (lambda (expr env k)
    (cond
      ((or (number? expr) (boolean? expr)) (k (cons expr (cons env '()))))
      ((not (pair? expr)) (k (cons (lookup expr env) (cons env '()))))
      ((null? (cdr expr)) (k (value (car expr) env (lambda (v) v))))
      ((eq? '= (car expr)) (k (pret-assign expr env)))
      (else (k (value (cadr expr) env
                   (lambda (val_cadr) (value (caddr expr) (cadr val_cadr) 
                                             (lambda (val_caddr) (cons ((getOp (car expr)) (car val_cadr) (car val_caddr))
                                                                       (cons (cadr val_caddr) '())))))))))))

(define getOp
  (lambda (op)
    (cond
      ((eq? '+ op) +)
      ((eq? '- op) -)
      ((eq? '* op) *)
      ((eq? '/ op) quotient)
      ((eq? '% op) remainder)
      ((eq? '|| op) (lambda (b1 b2) (or b1 b2)));for some reason just returning or gives a syntax error
      ((eq? '&& op) (lambda (b1 b2) (and b1 b2)));for some reason just returning and gives a syntax error
      ((eq? '! op) not)
      (else (getBool op)))))
     ; (else (error "error in getOp, operator not found")))))

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

;DO BOX STUFF STUART, ok ok, no need to yell
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
