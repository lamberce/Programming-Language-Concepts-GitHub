;;Chris Lambert
;;Assignment #14

;;Problem #2 written part
;;The fib-memo in the slides uses other previously stored values to compute its total while the
;;one for this assignment does not. For example, if fib-memo 4 and fib-memo 5 have been calculated
;;the fib-memo from class will use those stored values to calculate fib-memo 6 while the fib-memo from
;;this assignment will calculate fib 6 without using any previously stored values. 

;;Problem #1
;;Part a
(define member?-cps
	(lambda (e x k)
		(if (null? x)
			(k #f)
			(if (equal? (car x) e)
				(k #t)
				(member?-cps e (cdr x) k)))))
				
;;Part b
(define set?-cps
	(lambda (x k)
		(cond
			[(null? x) (k #t)]
			[(not (pair? x)) (k #f)]
			[else (member?-cps (car x) (cdr x) (lambda (y) (if y
																(k #f) 
																(set?-cps (cdr x) k))))])))
			
;;Part c
(define intersection-cps
	(lambda (ls1 ls2 k)
		(cond
			[(null? ls1) (k '())]
			[else (member?-cps (car ls1) ls2 (lambda (x)
												(if x
													(intersection-cps (cdr ls1) ls2 (lambda (x) (k (cons (car ls1) x))))
													(intersection-cps (cdr ls1) ls2 k))))])))
			
;;Part d
(define make-cps
	(lambda (x)
		(lambda (y k)
			(k (x y)))))
			
;;Part e
(define andmap-cps
	(lambda (pred-cps x k)
		(cond
			[(null? x) (k #t)]
			[else (pred-cps (car x) (lambda (y)
										(if (not y)
											(k #f)
											(andmap-cps pred-cps (cdr x) k))))])))
			
;;Part f
(define matrix?-cps
	(lambda (m k)
		(cond
			[(null? m) (k #f)]
			[(null? (car m)) (k #f)]
			[else ((make-cps list?) m (lambda (x)
					(if (not x)
						(k x)
						(andmap-cps (make-cps list?) m (lambda (y)
								(if (not y)
									(k #f)
									(andmap-cps (make-cps (lambda (L) (= ((make-cps length) L (lambda (x) x)) ((make-cps length) (car m) (lambda (x) x)))))
											(cdr m) (lambda (z) (k z)))))))))])))		
;;Problem #2
(define memoize
	(lambda (proc hash eq)
		(let ([table (make-hashtable hash eq)])
			(lambda x
				(if (hashtable-contains? table x)
					(hashtable-ref table x #f)
					(let ([val (apply proc x)])
							(begin (hashtable-set! table x val) val)))))))
							
			

;;Problem #3
(define subst-leftmost
	(lambda (new old sl pred)
		(call-with-values (lambda () (subst-leftmost-helper new old sl pred)) (lambda (x y) x))))
		
(define subst-leftmost-helper
	(lambda (new old x proc)
		(if (null? x)
			(values '() #f)
			(if (not (pair? (car x)))
				(if (proc (car x) old) 
					(values (cons new (cdr x)) #t) 
					(call-with-values (lambda () (subst-leftmost-helper new old (cdr x) proc)) 
						(lambda (sl y) (values (cons (car x) sl) y))))
				(call-with-values (lambda () (subst-leftmost-helper new old (car x) proc))
					(lambda (sl found)
						(if found
							(values (cons sl (cdr x)) #t)
							(call-with-values (lambda () (subst-leftmost-helper new old (cdr x) proc)) 
								(lambda (sl y) (values (cons (car x) sl) y))))))))))
				
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		