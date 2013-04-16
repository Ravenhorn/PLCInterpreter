(load "interpreter_environment.scm")

(define interpret-sl
  (lambda (ptree env class instance ret brk cont)
    (cond
      ((null? ptree) env)
      (else (interpret-sl (cdr ptree) (interpret-stmnt (car ptree) env class instance ret brk cont) class instance ret brk cont)))))

(define interpret-stmnt
  (lambda (stmnt env class instance ret brk cont)
    (cond
      ;((pair? (car stmnt)) (interpret-stmnt (car stmnt) env))
      ((eq? '= (car stmnt)) (pret-assign stmnt env (lambda (val env) env class instance)))
      ((eq? 'var (car stmnt)) (pret-declare stmnt env class instance))
      ((eq? 'if (car stmnt)) (pret-if stmnt env class instance ret brk cont))
      ((eq? 'return (car stmnt)) (ret (pret-return stmnt env class instance)))
      ((eq? 'while (car stmnt)) (pop-frame (pret-while stmnt (push-frame env) class instance ret)))
      ((eq? 'break (car stmnt)) (brk env))
      ((eq? 'continue (car stmnt)) (cont env))
      ((eq? 'begin (car stmnt)) (pop-frame (interpret-sl (cdr stmnt) (push-frame env) class instance ret brk cont)))
      ((eq? 'funcall (car stmnt)) (pret-funcall stmnt env class instance (lambda (retval) env)))
      ;((eq? 'dot (car stmnt)) (pret-dot stmnt env class instance))
      (else (error "invalid parse tree")))))

;k expects params class instance
(define pret-dot
  (lambda (stmnt env class instance k)
    (handle-left (cadr stmnt) env class instance (lambda (c i) (k c i)))))

(define handle-left
  (lambda (lhs env class instance k)
    (cond
      ((eq? 'super lhs) (k (get-parent class env) instance))
      ((eq? 'this lhs) (error "handle this")) ; handle this for next time
      (else (handle-left-helper (lookup lhs env class instance) k)))))

(define get-parent
  (lambda (name env)
    (lookup name env '() '())))

(define handle-left-helper
  (lambda (lookup_val k)
    (cond    
      ((eq? (instance? lookup_val) (k (get-class lookup_val) lookup_val)))
      (else (k lookup_val '())))))

(define instance?
  (lambda (to_check)
    (cond
      ((null? (cddr to_check)) #t)
      (else #f))))

(define pret-while
  (lambda (stmnt enviro class instance return)
    (call/cc (lambda (break)
               (letrec ((loop (lambda (cond body env)
                                (eval-if cond env class instance (lambda (if1 if_enviro)
                                                    (if  if1
                                                        (loop cond body (interpret-stmnt body if_enviro class instance return break (lambda (e) (break (loop cond body (pop-frame e))))))
                                                        env))))))
                  (loop (cadr stmnt) (caddr stmnt) enviro))))))

(define pret-return
  (lambda (stmnt env class instance)
    (value (cadr stmnt) env class instance (lambda (val enviro) 
                              (cond
                                ((eq? val #t) 'true)
                                ((eq? val #f) 'false)
                                (else val))))))

;need a pret-declare for static stuff?
(define pret-declare
  (lambda (stmnt env class instance)
    (cond
      ((null? stmnt) (error "null arg passed to declare"))
      ((null? (cddr stmnt)) (bind (cadr stmnt) '() env))
      (else (bind (cadr stmnt) (value (cddr stmnt) env class instance (lambda (val enviro) val)) (value (caddr stmnt) env class instance (lambda (val2 enviro2) enviro2)))))))

(define pret-assign
  (lambda(stmnt env class instance k)
    (cond
      ((null? stmnt) (error "null arg passed to assign"))
      ((null? (cddr stmnt)) (error "no value to assign"))
      ((declared? (cadr stmnt) env) (value (caddr stmnt) env class instance (lambda (val enviro) (k val (bind-deep (cadr stmnt) val enviro)))))
      (else (error "variable not declared")))))

(define pret-if
  (lambda (stmnt env class instance ret brk cont)
    (eval-if (cadr stmnt) env class instance
             (lambda (if1 enviro)
               (cond
                 ((null? (cdddr stmnt)) ;no else
                  (cond
                    (if1 (pop-frame (interpret-stmnt (caddr stmnt) (push-frame enviro) class instance ret brk cont)))
                    (else enviro))) 
                 (else ;has an else
                  (cond
                    (if1 (pop-frame (interpret-stmnt (caddr stmnt) (push-frame enviro) class instance ret brk cont)))
                    (else (pop-frame (interpret-stmnt (cadddr stmnt) (push-frame enviro) class instance ret brk cont))))))))))

(define eval-if
  (lambda (if env class instance k)
    (cond
      ((list? if)
       (value (cadr if) env class instance
              (lambda (val enviro)
                (cond
                  ((null? (cddr if)) (k ((getBool (car if)) val) enviro))
                  (else (value (caddr if) enviro class instance (lambda (val2 enviro2) (k ((getBool (car if)) val val2) enviro2))))))))
      (else
       (k if env)))))
                                 
(define value
  (lambda (expr env class instance k)
    (cond
      ((or (number? expr) (boolean? expr)) (k expr env))
      ((eq? expr 'false) (k #f env))
      ((eq? expr 'true) (k #t env))
      ((not (pair? expr)) (k (lookup expr env class instance) env))
      ((null? (cdr expr)) (value (car expr) env class instance (lambda (vals enviro) (k vals enviro))))
      ((eq? '= (car expr)) (pret-assign expr env class instance (lambda (vals enviro) (k vals enviro))))
      ((eq? 'funcall (car expr)) (k (pret-funcall expr env class instance (lambda (retval) retval)) env))
      ((eq? '! (car expr)) (value (cdr expr) env class instance (lambda (vals enviro) (k (not vals) enviro))))
      ((and (eq? '- (car expr)) (null? (cddr expr))) (value (cdr expr) env class instance (lambda (vals enviro) (k (* -1 vals) enviro))))
      (else (value (cadr expr) env class instance (lambda (val enviro) (value (caddr expr) enviro class instance 
                                             (lambda (val2 enviro2) (k ((getOp (car expr)) val val2) enviro2)))))))))

(define pret-func-def
  (lambda (stmnt env name)
    (bind (cadr stmnt)
          (cons (caddr stmnt) (cons (cadddr stmnt) (cons (lambda (v) (get-func-env v)) ;<--handle recursion
                                                         (cons (lambda (enviro) (get-func-class name enviro))'())))) env)))
(define pret-funcall
  (lambda (stmnt env class instance k)
    (k (call/cc (lambda (ret)
                  (cond
                    ((list? (cadr stmnt)) (pret-dot (cadr stmnt) env class instance (lambda (c i) (funcall-helper stmnt (get-method (caddr (cadr stmnt)) c) env class instance c i ret))))
                    (else
                     (cond
                       ((null? instance) (funcall-helper stmnt (get-method (cadr stmnt) class) env class instance (cadddr (get-method (cadr stmnt) class)) '() ret))
                        (else (funcall-helper stmnt (get-method (cadr stmnt) (get-instance-class instance)) env class instance (get-instance-class instance) instance ret))))))))))
               ;(interpret-sl (cadr (lookup (cadr stmnt) env class instance)) (setup-func-env stmnt env class instance) class instance ret (lambda (env) (error "break called outside of a loop")) (lambda (env)(error "continue called outside of a loop"))))))))

(define get-method
  (lambda (name class)
    (lookup name '() class '())))

(define funcall-helper
  (lambda (stmnt closure env old_class old_instance new_class new_instance ret)
    (interpret-sl (cadr closure) (setup-func-env stmnt closure env old_class old_instance new_class new_instance) new_class new_instance ret (lambda (env) (error "break called outside of a loop")) (lambda (env)(error "continue called outside of a loop")))))
    
(define setup-func-env
  (lambda (stmnt closure env old_class old_instance new_class new_instance)
    (assign-args (car closure) (cddr stmnt) ((caddr closure) env);this last arg returns a get-func-env procedure
                                                      env old_class old_instance new_class new_instance))) 

;TODO finish class/instance binding once bind works for class instances
(define assign-args
  (lambda (params args func_env old_env old_class old_instance new_class new_instance)
    (cond
      ((null? params) func_env)
      ;((list? (car params)) (handle-dot))
      ((eq? '& (car params)) (assign-args (cddr params) (cdr args) (bind-box (cadr params) (get-box-for-ref (car args) old_env) func_env) old_env old_class old_instance))
      (else (value (car args) old_env old_class old_instance (lambda (val env) (assign-args (cdr params) (cdr args) (bind (car params) val func_env) env old_class old_instance new_class new_instance)))))))

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

(define getOp
  (lambda (op)
    (cond
      ((eq? '+ op) +)
      ((eq? '- op) -)
      ((eq? '* op) *)
      ((eq? '/ op) quotient)
      ((eq? '% op) remainder)
      (else (getBool op)))))