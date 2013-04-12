;KALAA Interpreter
;Stuart Long and Jason Kuster
;EECS 345 Interpreter 3
(load "functionParser.scm")

(define interpret
  (lambda (filename)
    (call/cc (lambda (ret)
               (interpret-global-sl (parser filename) (new-env) (lambda (v) (interpret-sl (cadr (lookup 'main v)) v ret (lambda (env) (error "break called outside of a loop")) (lambda (env)(error "continue called outside of a loop")))))))))

(define interpret-global-sl
  (lambda (ptree env k)
    (cond
      ((null? ptree) (k env))
      (else (interpret-global-sl (cdr ptree) (interpret-global-stmnt (car ptree) env) (lambda (v) (k v)))))))

(define interpret-global-stmnt
  (lambda (stmnt env)
    (cond
      ((eq? 'var (car stmnt)) (pret-declare stmnt env))
      ((eq? 'function (car stmnt)) (pret-func-def stmnt env))
      (else (error "invalid global parse tree")))))

(define pret-func-def
  (lambda (stmnt env)
    (bind (cadr stmnt)
          (cons (caddr stmnt) (cons (cadddr stmnt) (cons (lambda (v) (get-func-env v)) ;<--handle recursion
                                                         '()))) env)))

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
  (lambda (var env class instance)
    (cond
      ((not (null? env)) (lookup-env var env (lambda (val)
                                               (cond
                                                 ((not (null? val)) (val))
                                                 (else (lookup-ci var class instance (lambda (val)
                                                                                       (cond
                                                                                         ((null? val) (error "Variable not declared"))
                                                                                         (else val)))))))))
      (else (lookup-ci var class instance (lambda (val)
                                            (cond
                                              ((null? val) (error "Variable not declared"))
                                              (else val))))))))

(define lookup-env
  (lambda (var env k)
    (cond
      ((null? env) '())
      ((not (null? (k (lookvar var (caar env) (cadar env)))) (lookvar var (caar env) (cadar env))))
      (else (k (lookup var (cdr env)))))))

(define lookvar
  (lambda (var varlist vallist)
    (cond
      ((null? varlist) '())
      ((eq? var (car varlist))
       (cond
         ((null? (unbox (car vallist))) (error "variable/method declared but not initialized"))
         (else (unbox (car vallist)))))
      (else (lookvar var (cdr varlist) (cdr vallist))))))

(define lookup-ci
  (lambda (var class instance)
    (cond
      (

(define bind
  (lambda (var val env)
    (cons (cons (cons var (caar env)) (cons (cons (box val) (cadar env)) '())) (cdr env))))

(define bind-deep
  (lambda (var val env)
    (cond
      ((null? env) (error "null environment"));shouldn't this error out?
      ((declared? var (cons (car env) '())) (handle-box var val (car env) (lambda (val enviro) env)))
      (else (cons (car env) (bind-deep var val (cdr env)))))))

(define handle-box
  (lambda (var val env k)
    (k (set-box! (get-box var (car env) (cadr env)) val) env)))

(define get-box
  (lambda (var vars vals)
    (cond
      ((null? vars) (error "couldn't find box"))
      ((eq? var (car vars)) (car vals))
      (else (get-box var (cdr vars) (cdr vals))))))

(define get-box-for-ref
  (lambda (var env)
    (cond
      ((null? env) (error "var not declared"))
      ((or (pair? var) (boolean? var) (number? var)) (error "only variables can be passed by reference"))
      ((declared? var (cons (car env) '())) (get-box var (caar env) (cadar env)))
      (else (get-box-for-ref var (cdr env))))))

(define bind-box
  (lambda (var box_val env)
    (cons (cons (cons var (caar env)) (cons (cons box_val (cadar env)) '())) (cdr env))))

(define declared?
  (lambda (var env)
    (cond
      ((null? env) #f)
      ((null? var) (error "null var"))
      ((eq? (car env) var) #t)
      ((and (list? (car env)) (declared? var (car env))) #t)
      (else (declared? var (cdr env))))))

(define get-func-env
  (lambda (env)
    (cond
      ((null? (cdr env)) env)
      (else (get-func-env (cdr env))))))
