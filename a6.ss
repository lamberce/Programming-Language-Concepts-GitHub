;;Chris Lambert
;;Assignment #6

;;Problem #1
(define curry2
	(lambda (x)
		(letrec ([func1
					(lambda (x y)
						(letrec ([func2
									(lambda (x y z)
										(x y z))])		
							(lambda (z) (func2 x y z))))])
			(lambda (y) (func1 x y)))))
			
;;Problem #2
(define curried-compose
	(lambda (x)
		(letrec ([func1
					(lambda (x y)
						(letrec ([func2
									(lambda (x y z)
										(x (y z)))])		
							(lambda (z) (func2 x y z))))])
			(lambda (y) (func1 x y)))))
	
;;Problem #3
(define compose
	(lambda functions
		(letrec ([func
					(lambda (functions x n)
						(cond
							[(= n 0) ((car functions) x)]
							[else (func functions ((list-ref functions n) x) (- n 1))]))])
			(lambda (x) func functions x (- (size functions) 1)))))
				
(define size
	(lambda (x)
		(if (equal? x '())
			0
			(+ 1 (size (cdr x))))))

;;Problem #4
(define make-list-c
	(lambda (n)
		(letrec ([func 
					(lambda (x n)
						(if (= n 0)
							'()
							(cons x (func x (- n 1)))))])
			(lambda (x) (func x n)))))
			
;;Problem #5
(define let->application
	(lambda (x)
		(cons (append (cons (quote lambda) (list (domain (list-ref x 1)))) (list (list-ref x 2)))
											(range (list-ref x 1)))))
			
		
(define domain
	(lambda (x)
		(let ([x (map first x)])
			x)))

(define first
	(lambda (x)
		(list-ref x 0)))
			
(define range
	(lambda (x)
		(let ([x (map cadr x)])
			x)))			

;;Problem #6
(define let*->let
	(lambda (x)
		(if (equal? (cdr (list-ref x 1)) '())
			(list (quote let) (list (list-ref (list-ref x 1) 0)) (list-ref x 2)) 
			(list (quote let)  (list (list-ref (list-ref x 1) 0))  (dealWithLetDef (cdr (list-ref x 1)) (list-ref x 2)))))) 
	
(define dealWithLetDef
	(lambda (x last)
		(if (equal? (cdr x) '())
			(list (quote let) (list (car x)) last)
			(list (quote let) (list (car x)) (dealWithLetDef (cdr x) last)))))
	
;;Problem #7
(define filter-in
	(lambda (pred x)
		(cond
			[(equal? x '()) '()]
			[(pred (car x)) (cons (car x) (filter-in pred (cdr x)))]
			[else (filter-in pred (cdr x))])))

;;Problem #8
(define filter-out
	(lambda (pred x)
		(cond
			[(equal? x '()) '()]
			[(pred (car x)) (filter-out pred (cdr x))]
			[else (cons (car x) (filter-out pred (cdr x)))])))

;;Problem #9			
(define sort-list-of-symbols
	(lambda (x)
		(map string->symbol (sort string<? (map symbol->string x)))))
		
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			