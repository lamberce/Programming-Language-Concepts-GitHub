

;<expression>::= <identifier>
;            ::= <number>
;            ::= (lambda (<identifier>) <expression>)
;            ::= (if <expression> <expression> <expression>)
;            ::= (<expression> <expression>)

(define scheme-value? (lambda (v) #t))

(define-datatype expression expression?
  (var-exp
   (id symbol?))
  (lit-exp
   (val scheme-value?))
  (lambda-exp
   (id scheme-value?)
   (body scheme-value?))
  (app-exp
   (operator expression?)
   (operand (list-of expression?)))
  (if-exp
   (condition expression?)
   (if-true expression?)
   (if-false expression?))
  (if-no-false-exp
   (condition expression?)
   (if-true expression?))
  (let-exp
   (var list?)
   (body pair?))
  (named-let-exp
   (name symbol?)
   (var list?)
   (body pair?))
  (letrec-exp
   (var list?)
   (body pair?))
  (let*-exp
   (var list?)
   (body pair?))
  (set-exp
   (var symbol?)
   (body pair?))
  (begin-exp
   (exps (list-of expression?)))
)


(define parse-exp
  (lambda (datum)
    (cond [(symbol? datum) (var-exp datum)]
	  [(number? datum) (lit-exp datum)]
	  [(vector? datum) (lit-exp datum)]
	  [(boolean? datum) (lit-exp datum)]
	  [(string? datum) (lit-exp datum)]
	  [(pair? datum)
	   (cond [(eq? (car datum) 'quote) (lit-exp (cadr datum))]
		 [(eq? (car datum) 'lambda)
		  (if (check-lambda datum)
		     (let ([body (cddr datum)]
			   [id (cadr datum)])
		       (lambda-exp id (map parse-expression body))))]
		 [(eq? (car datum) 'if)
		  (if (check-if datum)
		      (if (null? (cdddr datum))
			  (if-no-false-exp (parse-expression (cadr datum))
					   (parse-expression (caddr datum)))
			  (if-exp (parse-expression (cadr datum))
				  (parse-expression (caddr datum))
				  (parse-expression (cadddr datum)))))]
		 [(eq? (car datum) 'let)
		  (if (named-let? datum)
		      (if (check-let (cddr datum))
				(named-let-exp (cadr datum)
					 (map parse-vars (caddr datum))
					 (map parse-expression (cdddr datum))))
		      (if (check-let (cdr datum))
			  (let-exp (map parse-vars (cadr datum))
				   (map parse-expression (cddr datum)))))]
		 [(eq? (car datum) 'letrec)
		  (if (check-let (cdr datum))
		      (letrec-exp (map parse-vars (cadr datum))
				  (map parse-expression (cddr datum))))]
		 [(eq? (car datum) 'let*)
		  (if (check-let (cdr datum))
		      (let*-exp (map parse-vars (cadr datum))
				(map parse-expression (cddr datum))))]
		 [(eq? (car datum) 'set!)
		  (if (check-set datum)
		      (set-exp (cadr datum)
			       (map parse-expression (cddr datum))))]
		 [(eq? (car datum) 'begin)
		       (if (check-begin datum)
			   (begin-exp (map parse-expression (cdr datum))))]
		 [else (check-app datum)
		       (app-exp (parse-expression (car datum))
				(map parse-expression (cdr datum)))])]
	  [else (eopl:error 'parse-expression
			    "Invalid concrete syntac ~s" datum)])))


(define unparse-exp
  (lambda (exp)
    (cases expression exp
	   [var-exp (id) id]
	   [lit-exp (val) val]
	   [lambda-exp (id body)
		       (if (eq? 2 (length body))
			   (append (list 'lambda id)(list (unparse-expression body)))
			   (append (list 'lambda id)(cdr (unparse-expression body))))]
	   [app-exp (operator operand)
		    (append (list (unparse-expression operator))
			  (map unparse-expression operand))]
	   [if-exp (condition if-true if-false)
		   (list 'if
			 (unparse-expression condition)
			 (unparse-expression if-true)
			 (unparse-expression if-false))]
	   [if-no-false-exp (condition if-true)
			    (list 'if
				  (unparse-expression condition)
				  (unparse-expression if-true))]
	   [let-exp (var body)
		    (append (list 'let
			  (unparse-vars var))
			  (map unparse-expression body))]
	   [named-let-exp (name var body)
			 (append (list 'let (unparse-expression name)
				       (unparse-vars var))
				 (map unparse-expression body))]
	   [letrec-exp (var body)
		      (append (list 'letrec
			     (unparse-vars var))
			     (map unparse-expression body))]
	   [let*-exp (var body)
		    (append (list 'let*
			   (unparse-vars var))
			   (map unparse-expression body))]
	   [set-exp (var body)
		    (append (list 'set!
			  var)
			  (map unparse-expression body))]
	   [begin-exp (exps)
		      (append (list 'begin) 
			    (map unparse-expression exps))])))

(define check-begin
  (lambda (datum)
    (if (null? (cdr datum))
	(eopl:error 'parse-expression 
		    "begin missing expression ~s" datum)
	#t)))

 (define check-lambda
   (lambda (datum)
     (let ([id-part (cadr datum)]
	   [body-part (cddr datum)])
       (cond [(null? body-part) (eopl:error 'parse-expression
					    "incorrect length ~s" datum)]
	     [(not (list? id-part)) #t]
	     [(map-fail id-part symbol? '#t) (eopl:error 'parse-expression
						    "lambda argument list: formals must be symbols: ~s" id-part)]
	     [else #t]))))

(define check-if
  (lambda (datum)
      (cond [(> (length datum) 4) (eopl:error 'parse-expression
						 "if expression: should have (only) test, then, and else clauses: ~s" datum)]
	    [else #t])))

(define check-app
  (lambda (datum)
    (if (list? datum)
	#t
	(eopl:error 'parse-expression
		    "application ~s is not a proper list" datum))))

(define check-let
  (lambda (datum)
    (let ([body-part (cdr datum)])
      (cond [(null? body-part) (eopl:error 'parse-expression
					   "incorrect length: ~s" datum)]
	    [(not (list? (car datum))) (eopl:error 'parse-expression
						    "not all proper lists ~s" (cadr datum))]
	    [(map-fail (car datum) list? #t) (eopl:error 'parse-expression
							  "not all proper lists: ~s" (cadr datum))]
	    [(map-fail (car datum) length 2) (eopl:error 'parse-expression
							  "not all length 2: ~s" (cadr datum))]
	    [(map-fail (car datum) (lambda (x) (symbol? (car x))) #t)
	     (eopl:error 'parse-expression "first memembers must be symbols: ~s" (cadr datum))]
	    [else #t]))))

(define check-set
  (lambda (datum)
    (let ([var (cadr datum)]
	  [body (cddr datum)])
      (cond [(null? body) (eopl:error 'parse-expression
				      "missing expression: ~s" datum)]
	    [(not (null? (cdddr datum))) (eopl:error 'parse-expression
						   "Too many parts in set!: ~s" datum)]
	    [else #t]))))

(define map-fail
  (lambda (ls proc base)
    (let ([list (map proc ls)])
      (if (null? (remq base list))
	  #f
	  #t))))

(define parse-vars
  (lambda (vars)
    (if (null? vars)
	vars
	(cons (parse-expression (car vars)) (parse-vars (cdr vars))))))

(define unparse-vars
  (lambda (vars)
    (if (null? vars)
	vars
	(cons (map unparse-expression (car vars)) (unparse-vars (cdr vars))))))

(define named-let?
  (lambda (datum)
    (if (list? (cadr datum))
	#f
	(if (symbol? (cadr datum))
	    #t
	    (eopl:error 'parse-expression 
			"Invalid name for named lets: ~s" datum)))))
