;;Chris Lambert
;;Assignment 3

;;Problem #1
(define cross-product
	(lambda (v1 v2)
		(list (- (* (list-ref v1 1) (list-ref v2 2)) (* (list-ref v1 2) (list-ref v2 1)))
			  (- (* (list-ref v1 2) (list-ref v2 0)) (* (list-ref v1 0) (list-ref v2 2)))
			  (- (* (list-ref v1 0) (list-ref v2 1)) (* (list-ref v1 1) (list-ref v2 0))))))
			  
;;Problem #2
(define parallel?
	(lambda (v1 v2)
		(if (equal? (cross-product v1 v2) '(0 0 0)) #t #f)))
		
;;Problem #3
(define collinear?
	(lambda (p1 p2 p3)
		(if (parallel? (make-vec-from-points p1 p2) (make-vec-from-points p2 p3)) #t #f)))
	
(define make-vec-from-points
	(lambda (p1 p2)
		(list (- (list-ref p2 0) (list-ref p1 0)) (- (list-ref p2 1) (list-ref p1 1))
			  (- (list-ref p2 2) (list-ref p1 2)))))
			  
;;Problem #4
(define nearest-point 
	(lambda (p listP)
		(if (equal? (cdr listP) '()) (list-ref listP 0)
			(let ((c (nearest-point p (cdr listP))))
			(if (<= (distance p (list-ref listP 0)) (distance p c)) (list-ref listP 0)
				c)))))
				
(define distance
	(lambda (p1 p2)
		(vec-length (make-vec-from-points p1 p2))))

(define vec-length
	(lambda (x)
		(sqrt (+ (expt (list-ref x 0) 2) (expt (list-ref x 1) 2) (expt (list-ref x 2) 2)))))
		
;;Problem #5
(define intersection
	(lambda (s1 s2)
		(cond
			[(equal? s1 '()) '()]
			[(and (equal? (cdr s1) '()) (contains? (car s1) s2)) (list s1)]
			[(equal? (cdr s1) '()) (intersection (cdr s1) s2)]
			[(contains? (car s1) s2) (cons (car s1) (intersection (cdr s1) s2))]
			[else (intersection (cdr s1) s2)])))

(define contains?
	(lambda (x y)
		(if (equal? y '()) #f
			(if (equal? x (car y)) #t (contains? x (cdr y))))))

;;Problem #6
(define subset?
	(lambda (s1 s2)
		(if (equal? (size s1) (size (intersection s1 s2))) #t #f)))

(define size
	(lambda (x)
		(if (equal? x '())
			0
			(+ 1 (size (cdr x))))))

;;Problem #7
(define relation?
	(lambda (x)
		(cond
			[(not (set? x)) #f]
			[(equal? x '()) #t]
			[(alltrue? (map listSize2? x)) #t]
			[else #f]))) 
			
	
(define set?
	(lambda (x)
		(cond
			[(not (or (list? x) (null? x))) #f]
			[(equal? x '()) #t]
			[(contains? (car x) (cdr x)) #f]
			[else (set? (cdr x))])))
			
(define listSize2?
	(lambda (x)
		(if (not (or (list? x) (null? x))) 
			#f
			(if (equal? x '())
				#f
				(if (equal? (cdr x) '())
					#f
					(if (equal? (cdr (cdr x)) '())
						#t
						#f))))))

(define alltrue?
	(lambda (x)
		(if (equal? x '())
			#t
			(if (equal? (car x) #t)
				(alltrue? (cdr x))
				#f))))

;;Problem #8
(define domain
	(lambda (x)
		(if (not (relation? x)) #f
			(let ([x (map first x)])
				(create-domain-list x '())))))

(define first
	(lambda (x)
		(list (list-ref x 0))))

(define create-domain-list
	(lambda (x y)
		(cond
			[(equal? x '()) y]
			[(contains? (car (car x)) y) (create-domain-list (cdr x) y)]
			[else (create-domain-list (cdr x) (cons (car (car x)) y))])))
				
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			