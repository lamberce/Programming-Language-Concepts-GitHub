;;<program>	<graphic>	<form>*
;;<form>	<graphic>	<definition> | <expression>
;;<definition>	<graphic>	<variable definition> | (begin <definition>*)
;;<variable definition>	<graphic>	(define <variable> <expression>)
;;<expression>	<graphic>	<constant>
;;	|	<variable>
;;	|	(quote <datum>)
;;	|	(lambda <formals> <expression> <expression>*)
;;	|	(if <expression> <expression> <expression>)
;;	|	(set! <variable> <expression>)
;;	|	<application>
;;<constant>	<graphic>	<boolean> | <number> | <character> | <string>
;;<formals>	<graphic>	<variable>
;;	|	(<variable>*)
;;	|	(<variable> <variable>* . <variable>)
;;<application>	<graphic>	(<expression> <expression>*)
(define def?
  (lambda (ls)
    (or (symbol? ls) (list-of symbol? ls))))

(define-datatype procedure procedure?
  [primitive
   (id symbol?)]
  [closure-record
   (parameter def?)
   (body expression?)
   (env list?)])

(define-datatype continuation continuation?
  (halt-cont)
  (rep-cont)
  (cons-cont
   (v scheme-value?)
   (cont continuation?))
  (proc-cont
   (cont continuation?))
  (eval-exps-cont
   (exps (list-of expression?))
   (env scheme-value?)
   (cont continuation?))
  (if-cont
   (true-exp expression?)
   (false-exp expression?)
   (cont continuation?)
   (env list?))
  (if-true-cont
   (true-exp expression?)
   (cont continuation?)
   (env list?))
  (map-cont
   (proc scheme-value?)
   (ls list?)
   (cont continuation?))
  ;;(cons-cont
   ;;(a scheme-value?)
   ;;(cont continuation?))
  (applyh-cont
   (proc scheme-value?)
   (ls list?)
   (cont continuation?))
  (call/cc-cont
   (cont continuation?)))

(define apply-cont
  (lambda (cont val)
    (cases continuation cont
	   [halt-cont ()
		      val]
	   [eval-exps-cont (exps env cont)
			   (eval-exps exps (cons-cont val cont) env)]
	   [cons-cont (v cont)
		      (apply-cont cont (cons v val))]
	   [proc-cont (cont)
		      (apply-proc val cont)]
	   [if-cont (if-true-exp if-false-exp next-cont env)
		    (if val
			(eval-expression if-true-exp next-cont env)
			(eval-expression if-false-exp next-cont env))]
	   [if-true-cont (if-true-exp next-cont env)
		    (if val
			(eval-expression if-true-exp next-cont env))]
	   [map-cont (proc ls cont)
		     (map-helper proc ls cont)]
	   ;;[cons-cont (a cont)
	;;	        (apply-cont cont (cons val a))]
	   [applyh-cont (proc ls cont)
		       (apply-cont cont (apply-helper proc (list (cdar ls)) (apply-primitive-proc proc (list val (caar ls))) cont))]
	   [call/cc-cont (cont)
			 (cases procedure val
				[closure-record (parameter body env)
					 (eval-expression body cont 
							  (extend-env parameter (list (acontinuation cont)) env))]
				[else  (eopl:error 'apply-primitive-proc "Not a procedure ~s" val)])]
	   [rep-cont ()
		     val])))

(define *prim-proc-names* '(+ - * / add1 sub1 zero? not = < > <= >= cons car cdr caar cddr cadr cdar caaar caadr cadar cdaar caddr cdadr cddar cdddr list null? eq? equal? atom? length list->vector list? pair? procedure? vector->list vector make-vector vector-ref vector? number? symbol? set-car! set-cdr! vector-set! list map apply assq assv append exit))

(define reset-global-env
  (lambda ()
    (set! global-env
      (extend-env *prim-proc-names* 
		  (map primitive *prim-proc-names*)
		  (empty-env)))))

(define global-env
  (extend-env *prim-proc-names* 
	      (map primitive *prim-proc-names*)
	      (empty-env)))

(define eval-one-exp
  (lambda (exp)
    (top-level-eval (parse-expression exp))))


(define rep
  (lambda ()
    (display ">> ")
    (let ([input (read)])
      (if (equal? input '(exit))
	  (printf "Bye...~%")
	  (let* ([parse-tree (parse-expression input)]
		 [response (top-level-eval parse-tree)])
	    (pretty-print response)
	    (rep))))))

(define top-level-eval
  (lambda (form)
    (eval-expression form (halt-cont) (extend-env '(a b) '(3 4) global-env))))


(define eval-expression
  (lambda (exp cont env)
    (cases expression exp
	   [var-exp (id) (apply-cont cont (apply-env env id))]
	   [val-exp (val) (apply-cont cont val)]
	   [num-exp (val) (apply-cont cont val)]
	   [lambda-exp (ids body)
		      (apply-cont cont (closure-record ids body env))]
	   [lambda-spec-exp (id body) 
			    (apply-cont cont (closure-record (list id) body env))]
	   [if-exp (test-exp true-exp false-exp)
		   (eval-expression test-exp
				    (if-cont true-exp false-exp cont env)
 				    env)]
	   [if-true-exp (conditional if-true)
			(eval-expression conditional
					 (if-true-cont if-true cont env)
					 env)]
	   [let-exp (definitions body) (syntax-expand exp env)]
	   [named-let-exp (name definitions body) '#f]
	   [letrec-exp (definitions body) '#f]
	   [let*-exp (definitions body) (syntax-expand exp env)]
	   [set!-exp (id value) (let ([id id] [body (eval-expression value env)] [env env])
				      (if (not (null? env))
					  (if (member id (caar env))
					      (change-env env id body)
					      (set! global-env (list (cons (cons id (caar global-env)) (list->vector (cons body (vector->list (cdar global-env))))))))
					  (set! global-env (list (cons (cons id (caar global-env)) (list->vector (cons body (vector->list (cdar global-env)))))))))]
	   [define-exp (id value) (let ([id id] [body (eval-expression value env)] [env env])
				      (begin (if (not (null? env))
					  (if (member id (caar env))
					      (change-env env id body)
					      (set! global-env (list (cons (cons id (caar global-env)) (list->vector (cons body (vector->list (cdar global-env))))))))
					  (set! global-env (list (cons (cons id (caar global-env)) (list->vector (cons body (vector->list (cdar global-env))))))))
					     (if (procedure? value)
					     (cases procedure value
						    [primitive (id)]
						    [closure-record (id2 body2 env2)
								    (set! global-env (list (cons (cons id (caar global-env)) (list->vector (cons (make-closure id2 body2 global-env) (vector->list (cdar global-env)))))))]
								    ))))]
	   [begin-exp (body)
		      (eval-begin body cont env)]
	   [cond-exp (conditionals bodies) (syntax-expand exp env)]
	   [and-exp (bodies) (syntax-expand exp env)]
	   [or-exp (bodies) (syntax-expand exp env)]
	   [case-exp (obj conditionals bodies) (syntax-expand exp env)]
	   [while-exp (test-exp body)
		      (if (eval-expression test-exp env)
			  (begin (eval-expression body env)
				 (eval-expression (while-exp test-exp body) env)))]
	   [call/cc-exp (receiver)
			(eval-expression receiver
					 (call/cc-cont cont)
					 env)]
	   [app-exp (exps)
                    (eval-exps exps (proc-cont cont) env)])))

(define eval-exps
  (lambda (exps cont env)
    (if (null? exps)
	(apply-cont cont '())
	(eval-expression (car exps) (eval-exps-cont (cdr exps) env cont) env))))

(define eval-begin
  (lambda (ls cont env)
    (if (= (length ls) 1)
	(eval-expression (car ls) cont env)
	(begin (eval-expression (car ls) cont env) (eval-begin (cdr ls) cont env)))))

(define make-closure
  (lambda (id body)
    (closure-record id body)))

(define apply-proc
  (lambda (proc cont)
    (if (procedure? (car proc))
	(cases procedure (car proc)
	       [closure-record (id body env)
			       (eval-expression body cont (extend-env id (cdr proc) env))]
	       [acontinuation (cont)
			      (apply-cont cont (cadr args))]
	       [primitive (id)
			  (apply-primitive-proc id (cdr proc) cont)])
	proc)))

(define apply-primitive-proc
  (lambda (id args cont)
    (case id
      [(+) (apply-cont cont (apply + args))]
      [(-) (apply-cont cont (apply - args))]
      [(*) (apply-cont cont (apply * args))]
      [(/) (apply-cont cont (apply / args))]
      [(add1) (apply-cont cont (+ (car args) 1))]
      [(sub1) (apply-cont cont (- (car args) 1))]
      [(exit) (if (null? args)
		  (apply-cont (halt-cont) (void))
		  (apply-cont (rep-cont) (car args)))]
      
      [(zero?) (apply-cont cont (zero? (car args)))]
      [(not) (apply-cont cont (not (car args)))]
      [(=) (apply-cont cont (= (car args) (cadr args)))]
      [(<=) (apply-cont cont (<= (car args) (cadr args)))]
      [(>=) (apply-cont cont (>= (car args) (cadr args)))]
      [(>) (apply-cont cont (> (car args) (cadr args)))]
      [(<) (apply-cont cont (< (car args) (cadr args)))]
      [(null?) (apply-cont cont (null? (car args)))]
      [(eq?) (apply-cont cont (eq? (car args) (cadr args)))]
      [(equal?) (apply-cont cont (equal? (car args) (cadr args)))]
      [(atom?) (apply-cont cont (atom? (car args)))]
      [(length) (apply-cont cont (length (car args)))]

      [(list->vector) (apply-cont cont (list->vector (car args)))]
      [(list) (apply-cont cont args)]
      [(list?) (apply-cont cont (list? (car args)))]
      [(pair?) (apply-cont cont (pair? (car args)))]
      [(procedure?) (apply-cont cont (procedure? (car args)))]
      [(vector->list) (apply-cont cont (vector->list (car args)))]
      [(vector) (apply-cont cont (list->vector args))]
      [(make-vector) (apply-cont cont (make-vector (car args)))]
      [(vector-ref) (apply-cont cont (vector-ref (car args) (cadr args)))]
      [(vector?) (apply-cont cont (vector? (car args)))]
      [(number?) (apply-cont cont (number? (car args)))]
      [(symbol?) (apply-cont cont (symbol? (car args)))]
      [(set-car!) (apply-cont cont (set-car! (car args) (cadr args)))]
      [(set-cdr!) (apply-cont cont (set-cdr! (car args) (cadr args)))]
      [(vector-set!) (apply-cont cont (vector-set! (car args) (cadr args) (caddr args)))]

      [(cons) (apply-cont cont (cons (car args) (cadr args)))]
      [(car) (apply-cont cont (car (car args)))]
      [(cdr) (apply-cont cont (cdr (car args)))]

      [(caar) (apply-cont cont (caar (car args)))]
      [(cadr) (apply-cont cont (cadr (car args)))]
      [(cdar) (apply-cont cont (cdar (car args)))]
      [(cddr) (apply-cont cont (cddr (car args)))]

      [(caaar) (apply-cont cont (caaar (car args)))]
      [(caadr) (apply-cont cont (caadr (car args)))]
      [(cadar) (apply-cont cont (cadar (car args)))]
      [(cdaar) (apply-cont cont (cdaar (car args)))]
      [(caddr) (apply-cont cont (caddr (car args)))]
      [(cdadr) (apply-cont cont (cdadr (car args)))]
      [(cddar) (apply-cont cont (cddar (car args)))]
      [(cdddr) (apply-cont cont (cdddr (car args)))]

      [(map) (apply-cont (map-cont (cadar args) (cdr args) (halt-cont)) '())]
      [(apply) (apply-cont (map-cont (cadar args) (list (cdar (cdr args))) (caar (cdr args)) (halt-cont)))]
      [(assq) (apply-cont cont (assq (cadar args) (cdr args)))]
      [(assv) (apply-cont cont (assv (cadar args) (cdr args)))]
      [(append) (apply-cont cont (append (car args) (cdr args)))]
      [else (eopl:error 'apply-primitive-proc
			"Procedure ~s not found" id)])))

(define map-helper
  (lambda (proc ls cont)
    (if (null? (car ls))
	(apply-cont cont '())
	(map-helper proc (list (cdar ls)) (cons-cont (apply-primitive-proc proc (car ls) cont) cont)))))

(define apply-helper
  (lambda (proc ls accu cont)
    (if (null? (car ls))
	(apply-cont cont accu)
	(applyh-cont proc ls cont))))