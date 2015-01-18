; startng code for students

;#1 product-cps

; Provided code.  Do not modify it.
(define append-cps 
  (lambda (l1 l2 k)
    (if (null? l1)
	(k l2)
	(append-cps (cdr l1)
		    l2
		    (lambda (appended-cdr)
		      (k (cons (car l1)
			       appended-cdr)))))))

(define map-cps
  (lambda (proc-cps ls k)
    (if (null? ls)
	(k '())
	(proc-cps (car ls)
		  (lambda (proced-car)
		     (map-cps proc-cps (cdr ls)
			  (lambda (mapped-cdr)
			    (k (cons proced-car mapped-cdr)))))))))

(define list?-cps
  (lambda (ls k)
    (cond [(null? ls) (k #t)]
          [(not (pair? ls)) (k #f)]
          [else (list?-cps (cdr ls) k)])))

(define length-cps
  (lambda (ls k)
    (if (null? ls)
        (k 0)
        (length-cps (cdr ls)
                    (lambda  (len)
                      (k (+ 1 len)))))))

(define matrix?-cps
  (lambda (m k)
    (list?-cps 
     m
     (lambda (is-list?)
       (if (not is-list?)
           (k #f)
           (if (null? m)
               (k #f)
               (if (null? (car m))
		   (k #f)
                   (andmap-cps 
                    list?-cps 
                    m
                    (lambda (full-of-lists?)
                      (if (not full-of-lists?)
                          (k #f)
                          (andmap-cps 
                           (lambda  (L k)
                             (length-cps 
                              L
                              (lambda  (length-L)
                                (length-cps 
                                 (car m)
                                 (lambda  (length-car)
                                   (k (= length-L length-car)))))))
                           (cdr m) k)))))))))))

(define andmap-cps
  (lambda (pred-cps ls k)
    (if (null? ls)
	(k #t)
	(pred-cps (car ls)
		  (lambda (v)
		    (if v
			(andmap-cps pred-cps
				    (cdr ls)
                                    k)
			(k #f)))))))

; Write your code for #1 here.
(define product-cps
	(lambda (x y k)
		(if (null? y)
			(k '())
			(if (null? x)
				(k '())
				(product-cps (cdr x) y (lambda (g) (map-cps (lambda (s k) (k (list (car x) s))) y (lambda (l) (append-cps g l k)))))))))
			


;#2 expand-lets.  Insert your datatype, parse,  and un-parse code. Modify that code as needed.
;                 Then write the expand-lets procedure.

(define expand-lets
	(lambda (x)
		(cond 
			[(not (pair? x)) x]
			[(equal? (car x) 'lambda) (append (list 'lambda (cadr x)) (map expand-lets (cddr x)))]
			[(equal? (car x) 'let) (append (list (list 'lambda (map car (cadr x)) (expand-lets (caddr x)))) (map  expand-lets (map cadr (cadr x))))]
			[(equal? (car x) 'let-exp) (list 'app-exp (list 'lambda (map car (cadr x)) (map expand-lets (append (list (cadr (caaddr x))) (caddr (caaddr x))))) (map expand-lets (map cadr (cadr x))))]
			[(equal? (car x) 'if-exp) (list 'if-exp (expand-lets (cadr x)) (expand-lets (caddr x)) (expand-lets (cadddr x)))]
			[(equal? (car x) 'lambda-exp) (list (car x) (cadr x) (map expand-lets (cddr x)))]
			[(equal? (car x) 'app-exp) (list (car x) (expand-lets (cadr x)) (expand-lets (caddr x)))]
			[else x])))

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
		
		
			
			




;#3  lyst

; provided code.  Do not modify it
(define make-stack
 (lambda ()
  (let ([stk '()])
   (lambda (msg  . args ) 
    (case msg
      [(empty?) (null? stk)]
      [(push)   (set! stk (cons (car args) stk))]
      [(pop)    (if (null? stk)
		    (errorf 'pop "attempt to pop empty stack")
		    (let ([top (car stk)])
		      (set! stk (cdr stk))
		      top))]
      [(top)    (car stk)]
      [(display) (list '*stack* stk)]
      [else (errorf 'stack "illegal message to stack object: ~a" msg)])))))

;-------------------------Write your make-lyst code (and any helpers it needs) here

(define make-lyst
	(lambda ()
		(let ([left (make-stack)] [right (make-stack)] [size 0] [current-pos 0])
			(lambda (msg . args)
				(case msg
					[(length) size]
					[(shift) (if (not (= (car args) 0))
								(if (< (car args) 0)
									(letrec ([shift-loop-neg
												(lambda (n)
													(if (not (= n 0))
														(begin (set! current-pos (- current-pos 1)) (right 'push (left 'pop)) (shift-loop-neg (+ n 1)))))])
										(shift-loop-neg (car args)))
									(letrec ([shift-loop-pos
												(lambda (n)
													(if (not (= n 0))
														(begin (set! current-pos (+ current-pos 1)) (left 'push (right 'pop)) (shift-loop-pos (- n 1)))))])
										(shift-loop-pos (car args)))))]
					[(goto) (if (not (= (- (car args) current-pos) 0))
								(if (< (- (car args) current-pos) 0)
									(letrec ([shift-loop-neg
												(lambda (n)
													(if (not (= n 0))
														(begin (set! current-pos (- current-pos 1)) (right 'push (left 'pop)) (shift-loop-neg (+ n 1)))))])
										(shift-loop-neg (- (car args) current-pos)))
									(letrec ([shift-loop-pos
												(lambda (n)
													(if (not (= n 0))
														(begin (set! current-pos (+ current-pos 1)) (left 'push (right 'pop)) (shift-loop-pos (- n 1)))))])
										(shift-loop-pos (- (car args) current-pos)))))]
					[(get-current-item) (let ([temp (right 'pop)])
											(begin (right 'push temp) temp))]
					[(get-current-pos) current-pos]
					[(insert-at-current-pos) (begin (set! size (+ size 1)) (right 'push (car args)))]
					[(remove-current) (begin (set! size (- size 1)) (right 'pop))]
					[(replace-current-item) (begin (right 'pop) (right 'push (car args)))]
					[else (errorf 'stack "illegal message to lyst object: `a" msg)])))))
					



















