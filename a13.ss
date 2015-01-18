;;Chris Lambert
;;Assignment #13

;;Chris Lambert
;;Assignment #13

(define-datatype expression expression?  ; based on the simple expression grammar, EoPL-2 p6
  (var-exp
    (id symbol?))
  (if-exp
	(condition expression?)
	(case1 expression?)
	(case2 expression?))
  (lambda-exp
    (vars pos-val?)
    (body pos-val?))
  (app-exp
    (rator pos-val?)
    (rand pair-or-null?))
  (let-exp
    (var-names list?)
	(body pair?))
  (letrec-exp
	(var-names check-let-vars)
	(body pair?))
  (named-let-exp
	(name symbol?)
	(var-names list?)
	(body pair?))
  (let*-exp
	(var-namse list?)
	(body pair?))
  (set-exp
	(id symbol?)
	(ex expression?))
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
				

(define symbol-or-list-of-symbols?
	(lambda (x)
		(if (null? x) 
			#t
			(if (not (pair? x))
				(if (symbol? x) #t #f)
				(if (symbol? (car x))
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
			#f
			(if (or (equal? x (begin x)) (boolean? x) (number? x))
				#t
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
									[else (let-exp (cadr datum) (map parse-exp (cddr datum)))])
								(if (symbol? (cadr datum))
									(cond
										[(not (symbol? (cadr datum))) (eopl:error 'parse-exp "Incorrect name in named let" datum)]
										[(not (check-let-vars (caddr datum))) (eopl:error 'parse-exp "Incorrect variable declarations in named let" datum)]
										[else (named-let-exp (cadr datum) (caddr datum) (map parse-exp (cdddr datum)))])	
									(eopl:error 'parse-exp "Incorrect form to let" datum))))]
					[(equal? (car datum) 'lambda)
						(cond
							[(not (>= (length datum) 3)) (eopl:error 'parse-exp "Incorrect length in lambda" datum)]
							[(not (symbol-or-list-of-symbols? (cadr datum))) (eopl:error 'parse-exp "Error in variable" datum)]
							[else (lambda-exp (cadr datum) (map parse-exp (cddr datum)))])]
					[(equal? (car datum) 'if)
						(if (= (length datum) 4)
							(if-exp
								(parse-exp (cadr datum))
								(parse-exp (caddr datum))
								(parse-exp (cadddr datum)))
							(eopl:error 'parse-exp "Incorrect length in if" datum))]
					[(equal? (car datum) 'letrec)
						(cond
							[(< (length datum) 3) (eopl:error 'parse-exp "Incorrect length in letrec" datum)]
							[(not (check-let-vars (cadr datum))) (eopl:error 'parse-exp "Error in variable declarations in letrec" datum)]
							[else (letrec-exp (cadr datum) (map parse-exp (cddr datum)))])]
					[(equal? (car datum) 'set!)
						(cond
							[(not (= (length datum) 3)) (eopl:error 'parse-exp "Incorrect length in set!" datum)]
							[(not (symbol? (cadr datum))) (eopl:error 'parse-exp "Incorrect variable in set!" datum)]
							[else (set-exp (cadr datum) (parse-exp (caddr datum)))])]
					[(equal? (car datum) 'let*)
						(cond
							[(< (length datum) 3) (eopl:error 'parse-exp "Incorrect length in let*" datum)]
							[(not (check-let-vars (cadr datum))) (eopl:error 'parse-exp "Error in variable declarations in let*" datum)]
							[else (let*-exp (cadr datum) (map parse-exp (cddr datum)))])]
					[else (app-exp (car datum) (cdr datum))]))
			(else (eopl:error 'parse-exp "Invalid concrete syntax ~s" datum)))))

(define unparse-exp ; an inverse for parse-exp
  (lambda (x)
    (cond
		[(equal? (car x) 'var-exp) (cadr x)]
		[(equal? (car x) 'if-exp) (list 'if (unparse-exp (cadr x)) (unparse-exp (caddr x)) (unparse-exp (cadddr x)))]
		[(equal? (car x) 'lambda-exp) (append (list 'lambda (cadr x)) (map unparse-exp (caddr x)))]
		[(equal? (car x) 'app-exp) (cons (cadr x) (caddr x))]
		[(equal? (car x) 'let-exp) (append (list 'let (cadr x)) (map unparse-exp (caddr x)))]
		[(equal? (car x) 'letrec-exp) (append (list 'letrec (cadr x)) (map unparse-exp (caddr x)))]
		[(equal? (car x) 'named-let-exp) (append (list 'let (cadr x) (caddr x)) (map unparse-exp (cadddr x)))]
		[(equal? (car x) 'let*-exp) (append (list 'let* (cadr x)) (map unparse-exp (caddr x)))]
		[(equal? (car x) 'set-exp) (list 'set (cadr x) (caddr x))]
		[(equal? (car x) 'lit-exp) (cadr x)])))
		
		

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

(define unparse-exp ; an inverse for parse-exp
  (lambda (x)
    (cond
		[(null? x) x]
		[(equal? (car x) 'var-exp) (cadr x)]
		[(equal? (car x) 'if-exp) (list 'if (unparse-exp (cadr x)) (unparse-exp (caddr x)) (unparse-exp (cadddr x)))]
		[(equal? (car x) 'lambda-exp) (append (list 'lambda (cadr x)) (map unparse-exp (caddr x)))]
		[(equal? (car x) 'app-exp) (cons (unparse-exp (cadr x)) (unparse-exp (caddr x)))]
		[(equal? (car x) 'let-exp) (append (list 'let (cadr x)) (map unparse-exp (caddr x)))]
		[(equal? (car x) 'letrec-exp) (append (list 'letrec (cadr x)) (map unparse-exp (caddr x)))]
		[(equal? (car x) 'named-let-exp) (append (list 'let (cadr x) (caddr x)) (map unparse-exp (cadddr x)))]
		[(equal? (car x) 'let*-exp) (append (list 'let* (cadr x)) (map unparse-exp (caddr x)))]
		[(equal? (car x) 'set-exp) (list 'set (cadr x) (caddr x))]
		[(equal? (car x) 'lit-exp) (cadr x)]
		[else '()])))
		
		

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
			
			