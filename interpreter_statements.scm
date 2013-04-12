(load "interpreter_environment.scm")

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
      ((eq? 'while (car stmnt)) (pop-frame (pret-while stmnt (push-frame env) ret)))
      ((eq? 'break (car stmnt)) (brk env))
      ((eq? 'continue (car stmnt)) (cont env))
      ((eq? 'begin (car stmnt)) (pop-frame (interpret-sl (cdr stmnt) (push-frame env) ret brk cont)))
      ((eq? 'funcall (car stmnt)) (pret-funcall stmnt env (lambda (retval) env)))
      (else (error "invalid parse tree")))))

(define pret-while
  (lambda (stmnt enviro return)
    (call/cc (lambda (break)
               (letrec ((loop (lambda (cond body env)
                                (eval-if cond env (lambda (if1 if_enviro)
                                                    (if  if1
                                                        (loop cond body (interpret-stmnt body if_enviro return break (lambda (e) (break (loop cond body (pop-frame e))))))
                                                        env))))))
                  (loop (cadr stmnt) (caddr stmnt) enviro))))))

(define pret-return
  (lambda (stmnt env)
    (value (cadr stmnt) env (lambda (val enviro) 
                              (cond
                                ((eq? val #t) 'true)
                                ((eq? val #f) 'false)
                                (else val))))))

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
      ((declared? (cadr stmnt) env) (value (caddr stmnt) env (lambda (val enviro) (k val (bind-deep (cadr stmnt) val enviro)))))
      (else (error "variable not declared")))))

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
    (cond
      ((list? if)
       (value (cadr if) env
              (lambda (val enviro)
                (cond
                  ((null? (cddr if)) (k ((getBool (car if)) val) enviro))
                  (else (value (caddr if) enviro (lambda (val2 enviro2) (k ((getBool (car if)) val val2) enviro2))))))))
      (else
       (k if env)))))
                                      
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
      ((eq? '&& op) (lambda (b1 b2) (and b1 b2)));for some reason just returning and gives a syntax error
      ((eq? '! op) not)
      (else (error "invalid operator")))))

(define value
  (lambda (expr env k)
    (cond
      ((or (number? expr) (boolean? expr)) (k expr env))
      ((eq? expr 'false) (k #f env))
      ((eq? expr 'true) (k #t env))
      ((not (pair? expr)) (k (lookup expr env) env))
      ((null? (cdr expr)) (value (car expr) env (lambda (vals enviro) (k vals enviro))))
      ((eq? '= (car expr)) (pret-assign expr env (lambda (vals enviro) (k vals enviro))))
      ((eq? 'funcall (car expr)) (k (pret-funcall expr env (lambda (retval) retval)) env))
      ((eq? '! (car expr)) (value (cdr expr) env (lambda (vals enviro) (k (not vals) enviro))))
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

(define pret-funcall
  (lambda (stmnt env k)
    (k (call/cc (lambda (ret)
               (interpret-sl (cadr (lookup (cadr stmnt) env)) (setup-func-env stmnt env) ret (lambda (env) (error "break called outside of a loop")) (lambda (env)(error "continue called outside of a loop"))))))))

(define setup-func-env
  (lambda (stmnt env)
    (assign-args (car (lookup (cadr stmnt) env)) (cddr stmnt) ((caddr (lookup (cadr stmnt) env)) ;this last arg returns a get-func-env procedure
                                                               env) env))) 

(define assign-args
  (lambda (params args func_env old_env)
    (cond
      ((null? params) func_env)
      ((eq? '& (car params)) (assign-args (cddr params) (cdr args) (bind-box (cadr params) (get-box-for-ref (car args) old_env) func_env) old_env))
      (else (value (car args) old_env (lambda (val env) (assign-args (cdr params) (cdr args) (bind (car params) val func_env) env)))))))