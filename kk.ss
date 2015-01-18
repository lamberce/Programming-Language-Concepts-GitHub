;:  Single-file version of the interpreter.
;; Easier to submit to server, probably harder to use in the development process




;-------------------+
;                   |
;    DATATYPES      |
;                   |
;-------------------+
	
;; environment type definitions

(define scheme-value?
  (lambda (x) #t))

(define-datatype environment environment?
  [empty-env-record]
  [extended-env-record
	(syms (list-of symbol-or-list-of-symbols?))
	(vals (list-of scheme-value?))
	(env environment?)]
  [recursively-extended-env-record
	(proc-names (list-of symbol?))
	(idss (list-of (list-of symbol?)))
	(bodies (list-of expression?))
	(env environment?)])
   
 (define-datatype proc-val proc-val?
	[prim-proc
		(name symbol?)]
    [closure
		(vars scheme-value?)
		(bodies (list-of scheme-value?))
		(env environment?)])
 
;; Parsed expression datatypes
(define-datatype expression expression?  ; based on the simple expression grammar, EoPL-2 p6
  (var-exp
    (id scheme-value?))
  (if-exp-2
	(condition expression?)
	(case1 expression?)
	(case2 expression?))
  (if-exp-1
	(condition expression?)
	(case1 expression?))
  (lambda-exp
    (vars pos-val?)
    (body pos-val?))
  (define-exp
	(var symbol?)
	(value expression?))
  (app-exp
    (rator pos-val?)
    (rand pair-or-null?))
  (let-exp
    (var-names list?)
	(body pair?))
  (letrec-exp
	(var-names scheme-value?)
	(body pair?))
  (named-let-exp
	(name symbol?)
	(var-names list?)
	(body pair?))
  (let*-exp
	(var-name list?)
	(body pair?))
  (or-exp
	(vals scheme-value?))
  (set-exp
	(id symbol?)
	(ex expression?))
  (case-exp
	(key scheme-value?)
	(statements scheme-value?))
  (cond-exp
	(statements scheme-value?))
  (begin-exp
	(body scheme-value?))
  (while-exp
	(condition expression?)
	(body scheme-value?))
  (lit-exp
	(id literal?)))
	
(define pos-val?
	(lambda (x)	
		#t))

(define pair-or-null?
	(lambda (x)
		(or (pair? x) (null? x))))
		
(define expression-or-expression-list?
	(lambda (x)
		(if (null? x)
			#t
			(if (not (list? x))
				(expression? x)
				(andmap expression-or-expression-list? x)))))
	
(define check-app-exp
	(lambda (x)
		(or (procedure? x) (symbol? x))))
	
(define check-let-vars
	(lambda (x)
		(if (not (list? x))
			#f
			(andmap check-let-vars-helper x))))
			
(define check-let-vars-helper
	(lambda (x)
		(if (not (list? x))
			#f
			(if (not (= (length x) 2))
				#f
				(if (not (symbol? (car x)))
					#f
					(begin (parse-exp (cadr x)) #t))))))
				

(define symbol-or-list-of-symbols? ;with refs
	(lambda (x)
		(if (null? x) 
			#t
			(if (not (pair? x))
				(if (symbol? x) #t #f)
				(if (or (symbol? (car x)) (list? (car x)))
					(symbol-or-list-of-symbols? (cdr x))
					#f)))))
					
(define symbol-or-list-of-symbols-numbers?
	(lambda (x)
		(if (null? x) 
			#t
			(if (not (pair? x))
				(if (or (symbol? x) (number? x)) #t #f)
				(if (or (symbol? (car x)) (number? (car x)))
					(symbol-or-list-of-symbols-numbers? (cdr x))
					#f)))))
	
(define literal?
	(lambda (x)
		(if (pair? x)
			(if (equal? (car x) 'quote)
				#t
				(if (list-of-numbers? x)
					#t
					#f))
			(if (or (equal? x (begin x)) (boolean? x) (number? x))
				#t
				#f))))
				
(define list-of-numbers?
	(lambda (x)
		(if (null? x)
			#t
			(if (number? (car x))
				(list-of-numbers? (cdr x))
				#f))))
	
			

(define parse-exp
	(lambda (datum)
		(cond
			((symbol? datum) (var-exp datum))
			((literal? datum) (lit-exp datum))
			((list? datum)
				(cond
					[(equal? (car datum) 'quote) (lit-exp (cadr datum))]
					[(equal? (car datum) 'let)
						(if (< (length datum) 3)
							(eopl:error 'parse-exp "Incorrect length in let" datum)
							(if (not (symbol? (cadr datum)))
								(cond
									[(not (check-let-vars (cadr datum))) (eopl:error 'parse-exp "Incorrect variable declarations in let" datum)]
									[else (let-exp (map parse-second (cadr datum)) (map parse-exp (cddr datum)))])
								(if (symbol? (cadr datum))
									(cond
										[(not (symbol? (cadr datum))) (eopl:error 'parse-exp "Incorrect name in named let" datum)]
										[(not (check-let-vars (caddr datum))) (eopl:error 'parse-exp "Incorrect variable declarations in named let" datum)]
										[else (named-let-exp (cadr datum) (map parse-second (caddr datum)) (map parse-exp (cdddr datum)))])	
									(eopl:error 'parse-exp "Incorrect form to let" datum))))]
					[(equal? (car datum) 'lambda)
						(cond
							[(not (>= (length datum) 3)) (eopl:error 'parse-exp "Incorrect length in lambda" datum)]
							[(not (symbol-or-list-of-symbols? (cadr datum))) (eopl:error 'parse-exp "Error in variable" datum)]
							[else (lambda-exp (cadr datum) (map parse-exp (cddr datum)))])]
					[(equal? (car datum) 'if)
						(if (= (length datum) 4)
							(if-exp-2
								(parse-exp (cadr datum))
								(parse-exp (caddr datum))
								(parse-exp (cadddr datum)))
							(if (= (length datum) 3)
								(if-exp-1 (parse-exp (cadr datum))
										(parse-exp (caddr datum)))
								(eopl:error 'parse-exp "Incorrect length in if" datum)))]
					[(equal? (car datum) 'letrec)
						(cond
							[(< (length datum) 3) (eopl:error 'parse-exp "Incorrect length in letrec" datum)]
							[(not (check-let-vars (cadr datum))) (eopl:error 'parse-exp "Error in variable declarations in letrec" datum)]
							[else (letrec-exp (map parse-second (cadr datum)) (map parse-exp (cddr datum)))])]
					[(equal? (car datum) 'set!)
						(cond
							[(not (= (length datum) 3)) (eopl:error 'parse-exp "Incorrect length in set!" datum)]
							[(not (symbol? (cadr datum))) (eopl:error 'parse-exp "Incorrect variable in set!" datum)]
							[else (set-exp (cadr datum) (parse-exp (caddr datum)))])]
					[(equal? (car datum) 'cond) (cond-exp (map list (map parse-exp (map car (cdr datum))) (map parse-exp (map cadr (cdr datum)))))]
					[(equal? (car datum) 'case) (case-exp (parse-exp (cadr datum)) (map list (map car (cddr datum)) (map parse-exp (map cadr (cddr datum)))))]
					[(equal? (car datum) 'begin) (begin-exp (map parse-exp (cdr datum)))]
					[(equal? (car datum) 'while) (while-exp (parse-exp (cadr datum)) (map parse-exp (cddr datum)))]
					[(equal? (car datum) 'or) (or-exp (map parse-exp (cdr datum)))]
					[(equal? (car datum) 'let*)
						(cond
							[(< (length datum) 3) (eopl:error 'parse-exp "Incorrect length in let*" datum)]
							[(not (check-let-vars (cadr datum))) (eopl:error 'parse-exp "Error in variable declarations in let*" datum)]
							[else (let*-exp (map parse-second (cadr datum)) (map parse-exp (cddr datum)))])]
					[(equal? (car datum) 'define) (define-exp (cadr datum) (parse-exp (caddr datum)))]
					[else (app-exp (parse-exp (car datum)) (map parse-exp (cdr datum)))]))
			((contains? datum *prim-proc-names*) (prim-proc datum))
			(else (eopl:error 'parse-exp "Invalid concrete syntax ~s" datum)))))

(define unparse-exp ; an inverse for parse-exp
  (lambda (x)
    (cond
		[(equal? (car x) 'var-exp) (cadr x)]
		[(equal? (car x) 'if-exp-2) (list 'if (unparse-exp (cadr x)) (unparse-exp (caddr x)) (unparse-exp (cadddr x)))]
		[(equal? (car x) 'if-exp-1) (list 'if (unparse-exp (cadr x)) (unparse-exp (caddr x)))]
		[(equal? (car x) 'lambda-exp) (append (list 'lambda (cadr x)) (map unparse-exp (caddr x)))]
		[(equal? (car x) 'app-exp) (cons (cadr x) (caddr x))]
		[(equal? (car x) 'let-exp) (append (list 'let (cadr x)) (map unparse-exp (caddr x)))]
		[(equal? (car x) 'letrec-exp) (append (list 'letrec (cadr x)) (map unparse-exp (caddr x)))]
		[(equal? (car x) 'named-let-exp) (append (list 'let (cadr x) (caddr x)) (map unparse-exp (cadddr x)))]
		[(equal? (car x) 'let*-exp) (append (list 'let* (cadr x)) (map unparse-exp (caddr x)))]
		[(equal? (car x) 'set-exp) (list 'set (cadr x) (caddr x))]
		[(equal? (car x) 'lit-exp) (cadr x)])))
		
(define parse-second
	(lambda (x)
		(list (car x) (parse-exp (cadr x)))))

(define occurs-free? ; in parsed expression
  (lambda (var exp)
    (cases expression exp
      (var-exp (id) (eqv? id var))
      (lambda-exp (id body)
        (and (not (eqv? id var))
             (occurs-free? var body)))
      (app-exp (rator rand)
        (or (occurs-free? var rator)
            (occurs-free? var rand))))))	
	
; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.
	 
	 
	 
	
;; environment type definitions

(define scheme-value?
  (lambda (x) #t))



;-------------------+
;                   |
;    PARSER         |
;                   |
;-------------------+


; This is a parser for simple Scheme expressions, such as those in EOPL, 3.1 thru 3.3.

; You will want to replace this with your parser that includes more expression types, more options for these types, and error-checking.

; Procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)


;-------------------+
;                   |
;   ENVIRONMENTS    |
;                   |
;-------------------+





; Environment definitions for CSSE 304 Scheme interpreter.  Based on EoPL section 2.3

(define empty-env
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env)))
	
(define extend-env-recursively
	(lambda (proc-names idss bodies env)
		(recursively-extended-env-record proc-names idss bodies env)))

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (xsym) (eqv? sym xsym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
     ((null? ls) #f)
     ((pred (car ls)) 0)
     (else (let ((list-index-r (list-index pred (cdr ls))))
	     (if (number? list-index-r)
		 (+ 1 list-index-r)
		 #f))))))

(define apply-env
  (lambda (env sym succeed fail) ; succeed and fail are procedures applied if the var is or isn't found, respectively.
    (cases environment env
      (empty-env-record ()
        (fail))
      (extended-env-record (syms vals env)
		(let ((pos (list-find-position sym syms)))
      	  (if (number? pos)
	      (succeed (list-ref vals pos))
	      (apply-env env sym succeed fail))))
	  (recursively-extended-env-record (proc-names idss bodies old-env)
		(let ((pos (list-find-position sym proc-names)))
		  (if (number? pos)
		      (succeed (closure (list-ref idss pos) (list (list-ref bodies pos)) env))
			  (apply-env old-env sym succeed fail)))))))

(define mutate-env
	(lambda (env var setTo fail)
		(cases environment env
			(empty-env-record ()
				(fail))
			(extended-env-record (syms vals old-env)
				(let ((pos (list-find-position var syms)))
					(if (number? pos)
						(list-set! vals pos setTo)
						(mutate-env old-env var setTo fail))))
			(recursively-extended-env-record (proc-names idss bodies old-env)
				(let ((pos (list-find-position sym proc-names)))
					(if (number? pos)
						(list-set! bodies pos setTo)
						(mutate-env old-env var setTo fail)))))))
						
(define add-to-global-env
	(lambda (var value)
		(cases environment global-env
			(empty-env-record ()
				(fail))
			(extended-env-record (syms vals old-env)
				(let ((pos (list-find-position var syms)))
					(if (number? pos)
						(list-set! vals pos value)
						(set! global-env (extended-env-record (append syms (list var)) (append vals (list value)) old-env)))))
			(recursively-extended-env-record (proc-names idss bodies old-env)
				(fail)))))
					
(define reset-global-env 
	(lambda ()
		(set! global-env (extend-env            
							*prim-proc-names*
							(map prim-proc      
							*prim-proc-names*)
							(empty-env)))))
		
(define (list-set! list k val)
    (if (zero? k)
        (set-car! list val)
        (list-set! (cdr list) (- k 1) val)))

(define display-global
	(lambda ()
		(cases environment global-env
			(empty-env-record () 'null)
			(extended-env-record (syms vals old-env)
				(list syms))
			(recursively-extended-env-record (proc-names idss bodies old-env) 'null))))


;-----------------------+
;                       |
;   SYNTAX EXPANSION    |
;                       |
;-----------------------+



; To be added later

(define syntax-expand
	(lambda (exp)
		(cases expression exp
			[lit-exp (datum)
				(lit-exp datum)]
			[let-exp (var-names body)
				(app-exp (lambda-exp (map car var-names) (map syntax-expand body)) (map syntax-expand (map cadr var-names)))]
			[let*-exp (var-name body)
				(expand-to-lets var-name (map syntax-expand body))]
			[lambda-exp (vars body)
				(lambda-exp vars (map syntax-expand body))]
			[var-exp (id)
				(var-exp id)]
			[cond-exp (statements)
				(if (equal? (cadaar statements) 'else)
					(syntax-expand (cadar statements))
					(if-exp-2 (syntax-expand (caar statements)) 
							  (syntax-expand (cadar statements))
							  (syntax-expand (cond-exp (cdr statements)))))]
			[case-exp (key statements)
				(if (equal? (caar statements) 'else)
					(syntax-expand (cadar statements))
					(if-exp-2 (app-exp (var-exp 'ormap) (list (lambda-exp '(z)
															(list (app-exp (var-exp 'eqv?) (list (var-exp 'z) key)))) (lit-exp (caar statements))))
							  (syntax-expand (cadar statements))
							  (syntax-expand (case-exp key (cdr statements)))))]
			[begin-exp (body) (app-exp (lambda-exp '() (map syntax-expand body)) '())]
			[named-let-exp (name var-names body)
				(let-exp (list (list name (lambda-exp (append (map car var-names) (list name)) (map (lambda (x) (add-param name x)) (map syntax-expand body))))) (list (app-exp (var-exp name) (append (map syntax-expand (map cadr var-names)) (list (var-exp name))))))]  
			[letrec-exp (vars body) 
				(letrec-exp (map (lambda (x) (list (car x) (syntax-expand (cadr x)))) vars) (map syntax-expand body))]
			[or-exp (vals)
				(or-exp (map syntax-expand vals))]
			[while-exp (condition body) (while-exp (syntax-expand condition) (map syntax-expand body))]
			[if-exp-2 (condition case1 case2)
				(if-exp-2 (syntax-expand condition) (syntax-expand case1) (syntax-expand case2))]
			[if-exp-1 (condition case1)
				(if-exp-1 (syntax-expand condition) (syntax-expand case1))]
			[define-exp (var value)
				(define-exp var (syntax-expand value))]
			[set-exp (id ex) 
				(set-exp id (syntax-expand ex))]
			[app-exp (rator rands)
				(app-exp (syntax-expand rator) (map syntax-expand rands))]
			[else (eopl:error 'syntax-expand "error in syntax-expand" exp)])))

(define expand-to-lets
	(lambda (vars body)
		(if (null? vars)
			(let-exp vars body)
			(if (null? (cdr vars))
				(let-exp (list (car vars)) body)
				(let-exp (list (car vars)) (list (expand-to-lets (cdr vars) body)))))))
			
	
(define add-param
	(lambda (add-name exp)
		(cases expression exp
			[lit-exp (datum)
				(lit-exp datum)]
			[let-exp (var-names body)
				(let-exp var-names (map (lambda (x) (add-param add-name x)) body))]
			[lambda-exp (vars body)
				(lambda-exp vars (map (lambda (x) (add-param add-name x)) body))]
			[var-exp (id)
				(var-exp id)]
			[set-exp (id ex)
				(set-exp id (add-param add-name ex))]
			[while-exp (condition body)
				(while-exp condition body)]
			[if-exp-2 (condition case1 case2)
				(if-exp-2 (add-param add-name condition) (add-param add-name case1) (add-param add-name case2))]
			[if-exp-1 (condition case1)
				(if-exp-1 (add-param add-name condition) (add-param add-name case1))]
			[app-exp (rator rands)
				(if (equal? rator (var-exp add-name))
					(app-exp (add-param add-name rator) (append (map (lambda (x) (add-param add-name x)) rands) (list (var-exp add-name))))
					(app-exp (add-param add-name rator) (map (lambda (x) (add-param add-name x)) rands)))]
			[else (eopl:error 'add-param "error in add-param" body)])))
				

;-------------------+
;                   |
;   INTERPRETER    |
;                   |
;-------------------+

(define *prim-proc-names* '(+ - * add1 sub1 cons = < > <= >= / append list eq? equal? length list->vector list? pair? 
											vector->list vector? number? car cdr caar cadr cdar cddr caaar 
											caadr cadar cdaar caddr cdadr cddar cdddr procedure? not zero? null? symbol?
											set! set-car! set-cdr! vector-ref apply map vector vector-set! and eqv?
											ormap andmap quotient odd? even? list-tail assq))											
											
(define init-env         ; for now, our initial global environment only contains 
  (extend-env            ; procedure names.  Recall that an environment associates
     *prim-proc-names*   ;  a value (not an expression) with an identifier.
     (map prim-proc      
          *prim-proc-names*)
     (empty-env)))
	 
(define global-env init-env)

; top-level-eval evaluates a form in the global environment

(define top-level-eval
  (lambda (form)
    ; later we may add things that are not expressions.
    (eval-exp form (empty-env))))

; eval-exp is the main component of the interpreter

(define eval-exp
	(let ([identity-proc (lambda (x) x)])
	  (lambda (exp env)
		(cases expression exp
		  [lit-exp (datum)
				(if (not (pair? datum))
					datum
					(if (equal? (car datum) 'quote)
						(car (getRidOfQuote datum))
						datum))]
		  [var-exp (id)
				(apply-env env id ; look up its value.
					identity-proc ; procedure to call if id is in the environment 
					(lambda ()
						(apply-env global-env
							id
							identity-proc
							(lambda () (eopl:error 'apply-env ; procedure to call if id not in env
								"variable not found in environment: ~s"
								id)))))]
		  [if-exp-2 (condition case1 case2)
				(if (eval-exp condition env) (eval-exp case1 env) (eval-exp case2 env))]
		  [if-exp-1 (condition case1)
				(if (eval-exp condition env) (eval-exp case1 env))]
		  [let-exp (var-names body)
				(eval-inorder body (extend-env (map car var-names) (map (lambda (x) (eval-exp x env)) (map cadr var-names)) env))]
		  [lambda-exp (vars body)
				(closure vars body env)]
		  [letrec-exp (var-names body) 
				(eval-inorder body (extend-env-recursively (map car var-names) (map cadr (map cadr var-names)) (map caaddr (map cadr var-names)) env))]
		  [while-exp (condition body) 
				(do () ((not (eval-exp condition env))) (eval-inorder body env))]
		  [or-exp (vals)
				(my-or vals env)]
		  [set-exp (id ex)
				(let ((temp (eval-exp ex env)))
					(mutate-env env id temp (lambda ()
						(mutate-env 
							global-env
							id
							temp
							(lambda () (eopl:error 'apply-env ; procedure to call if id not in env
								"variable not found in environment: ~s"
								id))))))]
		  [define-exp (var value)
				(add-to-global-env var (eval-exp value env))]
		  [app-exp (rator rands)
				(let ([proc-value (eval-exp rator env)] [args (eval-rands rands env)])
					(apply-proc proc-value args))]
		  [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)]))))		  

(define eval-inorder
	(lambda (body env)
		(if (<= (length body) 1)
			(eval-exp (car body) env)
			(begin (eval-exp (car body) env) (eval-inorder (cdr body) env)))))


(define getRidOfQuote
	(lambda (x)
		(if (not (pair? x))
			x
			(if (equal? (car x) 'quote)
				(getRidOfQuote (cdr x))
				x))))
	  
	  
; evaluate the list of operands, putting results into a list

(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-exp x env)) rands)))

;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.  
;  User-defined procedures will be added later.

(define apply-proc
  (lambda (proc-value args)
    (cases proc-val proc-value
      [prim-proc (op) (apply-prim-proc op args)]
	  [closure (vars body env) 
			(if (symbol? vars)
					(eval-inorder body (extend-env (list vars) (list args) env))
				(if (list? vars)
					(eval-inorder (proc-replacer body vars args) (extend-env vars args env))
					(eval-inorder body (extend-env (make-vars-list-proper vars) (make-args-into-proper args (length-of-improper vars)) env))))]
			; You will add other cases
      [else (error 'apply-proc
                   "Attempt to apply bad procedure: ~s" 
                    proc-value)])))
					
(define proc-replacer
	(lambda (body vars args)
		(begin
	;;	(display body)
		(if (null? (list-shortener (map (lambda (x y) (if (list? x) (list (cadr x) y) 1)) vars args)))
			body
			(change-vars (car body) (list-shortener (map (lambda (x y) (if (list? x) (list (cadr x) y) 1)) vars args)))))))
		
(define list-shortener
	(lambda (li)
		(cond
		[(null? li) '()]
		[(list? (car li)) (cons (car li) (list-shortener (cdr li)))]
		[else (list-shortener (cdr li))])))

(define change-vars
	(lambda (exp li)
		(cases expression exp
			[lit-exp (datum)
				(lit-exp datum)]
			[let-exp (var-names body)
				(let-exp var-names (map (lambda (x) (change-vars x li)) body))]
			[lambda-exp (vars body)
				(lambda-exp vars (map (lambda (x) (change-vars x li)) body))]
			[var-exp (id)
				(var-exp (is-it-there id li))]
			[set-exp (id ex)
				(set-exp id (change-vars ex li))]
			[while-exp (condition body)
				(while-exp condition body)]
			[if-exp-2 (condition case1 case2)
				(if-exp-2 (change-vars condition li) (change-vars case1 li) (change-vars case2 li))]
			[if-exp-1 (condition case1)
				(if-exp-1 (change-vars condition li) (change-vars case1 li))]
			[app-exp (rator rands)
				(app-exp (change-vars rator li) (map (lambda (x) (change-vars x li)) rands))]
			[else (eopl:error 'add-param "error in changer" body)])))

(define is-it-there
	(lambda (sym li)
		(cond
		[(null? li) sym]
		[(equal? sym (caar li)) (cadar li)]
		[else (is-it-there sym (cdr li))])))
			
(define make-vars-list-proper
	(lambda (vars)
		(if (symbol? vars)
			(list vars)
			(cons (car vars) (make-vars-list-proper (cdr vars))))))
			
(define length-of-improper
	(lambda (x)
		(if (symbol? x)
			1
			(+ 1 (length-of-improper (cdr x))))))
			
(define make-args-into-proper
	(lambda (args n)
		(if (= n 1)
			(list args)
			(cons (car args) (make-args-into-proper (cdr args) (- n 1))))))
					
; Usually an interpreter must define each 
; built-in procedure individually.  We are "cheating" a little bit.

(define apply-prim-proc
  (lambda (prim-proc args)
    (case prim-proc
      [(+) (apply + args)]
      [(-) (apply - args)]
      [(*) (apply * args)]
      [(add1) (+ (1st args) 1)]
      [(sub1) (- (1st args) 1)]
      [(cons) (cons (1st args) (2nd args))]
      [(=) (apply = args)]
	  [(<) (< (1st args) (2nd args))]
	  [(>) (> (1st args) (2nd args))]
	  [(<=) (<= (1st args) (2nd args))]
	  [(>=) (>= (1st args) (2nd args))]
	  [(/) (apply / args)]
	  [(append) (apply append args)]
	  [(list) (apply list args)]
	  [(eq?) (eq? (1st args) (2nd args))]
	  [(equal?) (equal? (1st args) (2nd args))]
	  [(eqv?) (eqv? (1st args) (2nd args))]
	  [(length) (length  (1st args))]
	  [(list->vector) (list->vector (1st args))]
	  [(list?) (list? (1st args))]
	  [(pair?) (pair? (1st args))]
	  [(vector->list) (vector->list (1st args))]
	  [(vector-ref) (vector-ref (1st args) (2nd args))]
	  [(map) (map (lambda x (apply-proc (1st args) x)) (2nd args))]
	  [(apply) (apply (lambda x (apply-proc (1st args) x)) (2nd args))]
	  [(vector?) (vector? (1st args))]
	  [(vector) (apply vector args)]
	  [(number?) (number? (1st args))]
	  [(car) (car (1st args))]
	  [(cdr) (cdr (1st args))]
	  [(caar) (caar (1st args))]
	  [(cadr) (cadr (1st args))]
	  [(cdar) (cdar (1st args))]
	  [(cddr) (cddr (1st args))]
	  [(caaar) (caaar (1st args))]
	  [(caadr) (caadr (1st args))]
	  [(cadar) (cadar (1st args))]
	  [(cdaar) (cdaar (1st args))]
	  [(caddr) (caddr (1st args))]
	  [(cdadr) (cdadr (1st args))]
	  [(cddar) (cddar (1st args))]
	  [(cdddr) (cdddr (1st args))]
	  [(procedure?) (or (expression? (1st args)) (proc-val? (1st args)))]
	  [(not) (not (1st args))]
	  [(zero?) (zero? (1st args))]
	  [(null?) (null? (1st args))]
	  [(symbol?) (symbol? (1st args))]
	  [(set-car!) (set-car! (1st args) (2nd args))]
	  [(set-cdr!) (set-cdr! (1st args) (2nd args))]
	  [(vector-set!) (vector-set! (1st args) (2nd args) (3rd args))]
	  [(ormap) (ormap (lambda x (apply-proc (1st args) x)) (2nd args))]
	  [(andmap) (andmap (lambda x (apply-proc (1st args) x)) (2nd args))]
	  [(and) (andmap (lambda (x) x) args)]
	  [(quotient) (apply quotient args)]
	  [(odd?) (apply odd? args)] 
	  [(even?) (apply even? args)]
	  [(list-tail) (apply list-tail args)]
	  [(assq) (apply assq args)]
      [else (eopl:error 'apply-prim-proc "Bad primitive procedure name: ~s" prim-op)])))
	
(define my-or
	(lambda (x env)
		(if (null? x)
			#f
			(let ((temp (eval-exp (car x) env)))
			(if temp
				temp
				(my-or (cdr x) env))))))
	
(define apply-to-list
	(lambda (proc x)
		(apply (cadr proc) x)))

(define rep      ; "read-eval-print" loop.
  (lambda ()
    (display "--> ")
    ;; notice that we don't save changes to the environment...
    (let ([answer (top-level-eval (parse-exp (read)))])
      ;; TODO: are there answers that should display differently?
      (eopl:pretty-print answer) (newline)
      (rep))))  ; tail-recursive, so stack doesn't grow.

(define eval-one-exp
  (lambda (x) (top-level-eval (syntax-expand (parse-exp x)))))
  
  
 
;-------------------+
;                   |
;  Extra Code       |
;                   |
;-------------------+ 
 
(define contains?
	(lambda (x y)
		(if (equal? y '()) #f
			(if (equal? x (car y)) #t (contains? x (cdr y)))))) 

 
 
 
 
 ;;[while-exp (condition body) (letrec-exp (list (list 'a (lambda-exp '() (if-exp-1 condition (begin-exp (list body (var-exp 'a))))))) (list (app-exp (var-exp 'a) '())))]
 
 
 
 