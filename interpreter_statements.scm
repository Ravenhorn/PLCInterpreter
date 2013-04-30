(load "interpreter_environment.scm")

(define interpret-sl
  (lambda (ptree env class instance ret brk cont throw)
    (cond
      ((null? ptree) env)
      (else (interpret-sl (cdr ptree) (interpret-stmnt (car ptree) env class instance ret brk cont throw) class instance ret brk cont throw)))))

(define interpret-stmnt
  (lambda (stmnt env class instance ret brk cont throw)
    (cond
      ;((pair? (car stmnt)) (interpret-stmnt (car stmnt) env))
      ((eq? '= (car stmnt)) (pret-assign stmnt env  class instance (lambda (val env) env)))
      ((eq? 'var (car stmnt)) (pret-declare stmnt env class instance))
      ((eq? 'if (car stmnt)) (pret-if stmnt env class instance ret brk cont throw))
      ((eq? 'return (car stmnt)) (ret (pret-return stmnt env class instance)))
      ((eq? 'while (car stmnt)) (pop-frame (pret-while stmnt (push-frame env) class instance ret throw)))
      ((eq? 'break (car stmnt)) (brk env))
      ((eq? 'continue (car stmnt)) (cont env))
      ((eq? 'begin (car stmnt)) (pop-frame (interpret-sl (cdr stmnt) (push-frame env) class instance ret brk cont throw)))
      ((eq? 'funcall (car stmnt)) (pret-funcall stmnt env class instance (lambda (retval) env)))
      ((eq? 'try (car stmnt)) (pop-frame (pret-try stmnt env class instance ret brk cont throw)))
      ((eq? 'throw (car stmnt)) (throw (value (cadr stmnt) env class instance (lambda (val env) val))))
      ;((eq? 'dot (car stmnt)) (pret-dot stmnt env class instance))
      (else (error "invalid parse tree")))))

(define pret-try 
  (lambda (stmnt env class instance ret brk cont throw)
    (call/cc (lambda (new_throw)
               (cond
                 ((null? (caddr stmnt)) (pret-try-no-catch stmnt env class instance ret brk cont throw new_throw))
                 ((null? (cadddr stmnt)) (pret-try-no-finally stmnt env class instance ret brk cont throw new_throw))
                 (else (pret-try-both stmnt env class instance ret brk cont throw new_throw)))))))

      ;         (interpret-sl (cadr stmnt) (push-frame env) class instance ret brk cont
       ;                      (cond
           ;                    ((null? (caddr stmnt)) (lambda (v) (new_throw (interpret-sl (cadr (cadddr stmnt)) (push-frame env) class instance ret brk cont throw))))
        ;                       ((null? (cadddr stmnt)) (lambda (v) (new_throw (interpret-sl (caddr (caddr stmnt)) (add-exception-val v (push-frame env)) class instance ret brk cont throw))))
         ;                      (else (lambda (v) (new_throw (interpret-sl (cadr (cadddr stmnt)) 
          ;                                                                (push-frame (pop-frame (interpret-sl (caddr (caddr stmnt)) (add-exception-val v (push-frame env)) class instance ret brk cont throw)))
            ;                                                              class instance ret brk cont throw))))))
(define pret-try-no-catch
  (lambda (stmnt env class instance ret brk cont old_throw new_throw)
    (let ((finally (lambda (continuation) (continuation (interpret-sl (cadr (cadddr stmnt)) (push-frame env) class instance ret brk cont old_throw)))))
           (interpret-sl (cadr stmnt) (push-frame env) class instance (finally ret) (finally brk) (finally cont) (finally new_throw)))))

(define pret-try-no-finally
  (lambda (stmnt env class instance ret brk cont old_throw new_throw)
    (interpret-sl (cadr stmnt) (push-frame env) class instance ret brk cont (lambda (v) (new_throw (interpret-sl (caddr (caddr stmnt)) (add-exception-val v (push-frame env)) class instance ret brk cont old_throw))))))

(define pret-try-both
  (lambda (stmnt env class instance ret brk cont old_throw new_throw)
    (let ((finally (lambda (continuation) (continuation (interpret-sl (cadr (cadddr stmnt)) (push-frame env) class instance ret brk cont old_throw)))))
    (interpret-sl (cadr stmnt) (push-frame env) class instance (finally ret) (finally brk) (finally cont) (lambda (v) (new_throw (interpret-sl (cadr (cadddr stmnt)) 
                                                                          (push-frame (pop-frame (interpret-sl (caddr (caddr stmnt)) (add-exception-val v (push-frame env)) class instance ret brk cont old_throw)))
                                                                          class instance ret brk cont old_throw)))))))
;TODO
;(define add-exception-val
  ;(lambda (e env)

;k expects params class instance
(define pret-dot
  (lambda (stmnt env class instance k)
    (handle-left (cadr stmnt) env class instance (lambda (c i) (k c i)))))

(define handle-left
  (lambda (lhs env class instance k)
    (cond
      ((eq? 'super lhs) (k (get-parent class) instance))
      ((eq? 'this lhs) (error "handle this")) ; handle this for next time
      (else (handle-left-helper (lookup lhs env class instance) k)))))

(define get-parent
  (lambda (class)
    (cadddr class)))

(define handle-left-helper
  (lambda (lookup_val k)
    (cond    
      ((instance? lookup_val) (k (get-class lookup_val) lookup_val))
      (else (k lookup_val '())))))

(define instance?
  (lambda (to_check)
    (cond
      ((null? (cddr to_check)) #t)
      (else #f))))

(define pret-while
  (lambda (stmnt enviro class instance return throw)
    (call/cc (lambda (break)
               (letrec ((loop (lambda (cond body env)
                                (eval-if cond env class instance (lambda (if1 if_enviro)
                                                    (if  if1
                                                        (loop cond body (interpret-stmnt body if_enviro class instance return break (lambda (e) (break (loop cond body (pop-frame e)))) throw))
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
      ((list? (cadr stmnt)) (pret-dot (cadr stmnt) env class instance (lambda (c i)
                                                                     (bind-deep (caddr (cadr stmnt)) (value (caddr stmnt) env class instance (lambda (v) v)) (car c))))) ;add a cond for objects
      ((declared? (cadr stmnt) env class) (value (caddr stmnt) env class instance (lambda (val enviro) (k val (bind-deep (cadr stmnt) val (cons (caar class) enviro)) ))))
      (else (begin (display "error on: ") (display stmnt) (newline) (error "variable not declared"))))))

(define pret-if
  (lambda (stmnt env class instance ret brk cont throw)
    (eval-if (cadr stmnt) env class instance
             (lambda (if1 enviro)
               (cond
                 ((null? (cdddr stmnt)) ;no else
                  (cond
                    (if1 (pop-frame (interpret-stmnt (caddr stmnt) (push-frame enviro) class instance ret brk cont throw)))
                    (else enviro))) 
                 (else ;has an else
                  (cond
                    (if1 (pop-frame (interpret-stmnt (caddr stmnt) (push-frame enviro) class instance ret brk cont throw)))
                    (else (pop-frame (interpret-stmnt (cadddr stmnt) (push-frame enviro) class instance ret brk cont throw))))))))))

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
      ((eq? 'dot (car expr)) (pret-dot expr env class instance (lambda (c i) (k  (lookup (caddr expr) '() c i) env))))
      ((eq? 'new (car expr)) (k (new-inst (cadr expr) (cddr expr) env class instance)))
      ((and (eq? '- (car expr)) (null? (cddr expr))) (value (cdr expr) env class instance (lambda (vals enviro) (k (* -1 vals) enviro))))
      (else (value (cadr expr) env class instance (lambda (val enviro) (value (caddr expr) enviro class instance 
                                             (lambda (val2 enviro2) (k ((getOp (car expr)) val val2) enviro2)))))))))

(define pret-func-def
  (lambda (stmnt env name)
    (bind (cadr stmnt)
          (cons (caddr stmnt) (cons (cadddr stmnt) (cons (lambda (v) (get-func-env v)) ;<--handle recursion
                                                         (cons (lambda (enviro) (get-func-class name enviro))'())))) env)))

;(define pret-const-def
;  (lambda (stmnt env name)
;    (bind (cadr stmnt)
;          (cons (caddr stmnt) (cons (cadddr stmnt) (cons (lambda (v) (get-func-env v))
;                                                         (cons (lambda (enviro) (get-func-class name enviro)) '())))) (cdr env))))

(define pret-funcall
  (lambda (stmnt env class instance k)
    (k (call/cc (lambda (ret)
                  (cond
                    ((list? (cadr stmnt)) (pret-dot (cadr stmnt) env class instance (lambda (c i) (funcall-helper stmnt (get-method (caddr (cadr stmnt)) c (cddr stmnt)) env class instance c i ret))))
                    (else
                     (cond
                       ((null? instance) (funcall-helper stmnt (get-method (cadr stmnt) class (cddr stmnt)) env class instance ((cadddr (get-method (cadr stmnt) class (cddr stmnt))) env) '() ret))
                        (else (funcall-helper stmnt (get-method (cadr stmnt) (get-instance-class instance) (cddr stmnt)) env class instance (get-instance-class instance) instance ret))))))))))
               ;(interpret-sl (cadr (lookup (cadr stmnt) env class instance)) (setup-func-env stmnt env class instance) class instance ret (lambda (env) (error "break called outside of a loop")) (lambda (env)(error "continue called outside of a loop"))))))))

(define get-method
  (lambda (name class args)
    (lookup-method name class (length args))))
    ;(lookup name '() class '())))

(define get-const
  (lambda (class args)
    (lookup-method (car (cddddr class)) class args)))

(define funcall-helper ;Interprets statement list of the function
  (lambda (stmnt closure env old_class old_instance new_class new_instance ret)
     (interpret-sl (cadr closure) (setup-func-env stmnt closure env old_class old_instance) new_class new_instance ret (lambda (env) (error "break called outside of a loop")) (lambda (env)(error "continue called outside of a loop")) (lambda (v) (error "throw called outside try")))))
    
(define setup-func-env
  (lambda (stmnt closure env old_class old_instance)
    (cond
      ((eq? (length (car closure)) (length (cddr stmnt)))
       (assign-args (car closure) (cddr stmnt) ((caddr closure) env);this last arg returns a get-func-env procedure
                                                      env old_class old_instance))
      (else (begin (display "error on: ") (display stmnt) (newline) (error "invalid arguments for function"))))))

;TODO finish class/instance binding once bind works for class instances
(define assign-args
  (lambda (params args func_env old_env old_class old_instance)
     (cond
      ((null? params) func_env)
      ;((list? (car params)) (handle-dot))
      ((eq? '& (car params)) (assign-args (cddr params) (cdr args) (bind-box (cadr params) (get-box-for-ref (car args) old_env) func_env) old_env old_class old_instance))
      (else (value (car args) old_env old_class old_instance (lambda (val env) (assign-args (cdr params) (cdr args) (bind (car params) val func_env) env old_class old_instance)))))))

(define new-inst
  (lambda (name args env class instance)
    (pret-const name args (get-own-class) (box (cons '() (cons ))))))

(define pret-const
  (lambda (args class instance)
    (cond
      ((null? (cadddr class)) (interpret-sl (cadr (get-const class args)) (new-env) class (get-inst-env (cadadr class) '() class instance) (error "ret") (error "brk") (error "cont") (error "throw"))) ;pretend that this returns the instance
      (else (begin (pret-const args (cadddr class) instance)
                          ((interpret-sl (cadr (get-const class args)) (new-env) class (get-inst-env (cadadr class) '() class instance) (error "ret") (error "brk") (error "cont") (error "throw"))))))))

(define get-inst-env
  (lambda (inst-exprs inst-vals class instance k)
    (cond
      ((null? inst-exprs) (k inst-vals))
      (else (value (car inst-exprs) (new-env) class instance (lambda (v e) (get-inst-env (cdr inst-exprs) class cons  (lambda (i2) (k (append i2 i1))))))))))

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
      (else (begin (display "error on: ") (display op) (newline) (error "invalid operator"))))))

(define getOp
  (lambda (op)
    (cond
      ((eq? '+ op) +)
      ((eq? '- op) -)
      ((eq? '* op) *)
      ((eq? '/ op) quotient)
      ((eq? '% op) remainder)
      (else (getBool op)))))
