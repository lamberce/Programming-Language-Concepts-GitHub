;;Chris Lambert
;;Assignment #5

;;Problem #1
(define minimize-interval-list
	(lambda (x)
		(minimize-interval-list-helper x 0 0 (size x))))

(define minimize-interval-list-helper
	(lambda (x m n p)
		(if (equal? x '())
			'()
			(cond
				[(= m p) x]
				[(= n p) (minimize-interval-list-helper x (+ m 1) 0 p)]
				[(= m n) (minimize-interval-list-helper x m (+ n 1) p)]
				[else 
					(if (interval-intersects? (list-ref x m) (list-ref x n))
						(minimize-interval-list-helper (remove-first (list-ref x n) (remove-first (list-ref x m) (cons (interval-union (list-ref x m) (list-ref x n)) x))) 0 0 (- p 1))
						(minimize-interval-list-helper x m (+ n 1) p))]))))
					
(define interval-intersects?
  (lambda (x y)
	(if (<= (list-ref x 0) (list-ref y 0))
		(if (>= (list-ref x 1) (list-ref y 0)) #t #f)
		(if (>= (list-ref y 1) (list-ref x 1)) #t #f))))  

(define interval-union
  (lambda (x y)
    (if (interval-intersects? x y)
        (if (<= (list-ref x 0) (list-ref y 0))
            (if (<= (list-ref x 1) (list-ref y 1)) (list (list-ref x 0) (list-ref y 1))
                (list (list-ref x 0) (list-ref x 1)))
            (if (<= (list-ref x 1) (list-ref y 1)) (list (list-ref y 0) (list-ref y 1))
                (list (list-ref y 0) (list-ref x 1))))
        x)))
		
;;Problem #2
(define exists?
	(lambda (x y)
		(not (allFalse? (map x y)))))
		
(define allFalse?
	(lambda (x)
		(if (equal? x '())
			#t
			(if (not (car x))
				(allFalse? (cdr x))
				#f))))
				
;;Problem #3
(define list-index
	(lambda (pred x)
		(list-index-helper pred x 0)))
		
(define list-index-helper
	(lambda (pred x loc)
		(if (equal? x '())
			#f
			(if (pred (car x)) 
				loc
				(list-index-helper pred (cdr x) (+ loc 1))))))

;;Problem #4
(define pascal-triangle
	(lambda (n)
		(if (< n 0)
			'()
			(if (= n 0)
				(list (list 1))
				(cons (make-pascal-entries n n) (pascal-triangle (- n 1))))))	)

(define make-pascal-entries
	(lambda (n k)
		(if (= k 0)
			(list 1)
			(cons (choose n k) (make-pascal-entries n (- k 1))))))
		
(define fact
  (lambda (x)
    (if (zero? x)	1
		(* x (fact (- x 1))))))
		
(define choose
	(lambda (x y)
		(/ (fact x) (* (fact (- x y)) (fact y)))))

;;Problem #5
(define product
	(lambda (s1 s2)
		(if (equal? s1 '())
			'()
			(if (equal? s2 '())
				'()
				(mergelists (group (car s1) s2) (product (cdr s1) s2))))))
			
(define group
	(lambda (e x)
		(if (equal? x '())
			'()
			(cons (list e (car x)) (group e (cdr x))))))
			
(define mergelists
	(lambda (l1 l2)
		(if (equal? l2 '()) 
			l1
			(mergelists (cons (car l2) l1) (cdr l2)))))

;;Problem #6
(define max-edges
	(lambda (n)
		(/ (* n (- n 1)) 2)))

;;Problem #7
(define complete?
	(lambda (x)
		(if (equal? x '())
			#t
			(if (all-same? (map size (map car (map cdr x))) (- (size x) 1))
				#t
				#f))))

(define all-same?
	(lambda (x n)
		(if (equal? x '())
			#t
			(if (= (car x) n)
				(all-same? (cdr x) n)
				#f))))
			
(define size
	(lambda (x)
		(if (equal? x '())
			0
			(+ 1 (size (cdr x))))))

;;Problem #8
(define complete
	(lambda (x)
		(complete-helper x 0 (size x))))
		
(define complete-helper
	(lambda (x n p)
		(if (= p 0)
			'()
			(if (<= (- p 1) n)
				(list (list (car x) (cdr x)))
					(let ([y (list-ref x (+ n 1))])
						(cons (list (car x) (cdr x)) 
							(complete-helper (replace-one (car x) y (replace-one (list-ref x (+ n 1)) (car x) x)) 
							(+ n 1) p)))))))
					
(define replace-one
	(lambda (old new x)
		(cond
			[(equal? '() x) '()]
			[(equal? '() (cdr x))
				(if (equal? (car x) old) (list new) x)]
			[(equal? (car x) old) (cons new (cdr x))]
			[else (cons (car x) (replace-one old new (cdr x)))]))) 

;;Problem #9
(define replace
	(lambda (old new x)
		(cond
			[(equal? '() x) '()]
			[(equal? '() (cdr x))
				(if (equal? (car x) old) (list new) x)]
			[(equal? (car x) old) (cons new (replace old new (cons new (cdr (cdr x)))))]
			[(equal? old (car (cdr x))) (cons (car x) (replace old new (cons new (cdr (cdr x)))))]
			[else (cons (car x) (replace old new (cdr x)))]))) 

;;Problem #10
(define remove-first
	(lambda (e x)
		(cond
			[(equal? '() x)	'()]
			[(equal? '() (cdr x)) 
				(if (equal? (car x) e) '() x)]
			[(equal? e (car (cdr x))) (cons (car x) (cdr (cdr x)))]
			[else (cons (car x) (remove-first e (cdr x)))])))

;;Problem #11
(define remove-last
	(lambda (e x)
		(cond
			[(equal? '() x) '()]
			[(equal? '() (cdr x)) 
				(if (equal? (car x) e) '() x)]
			[(equal? e (car (cdr x)))
				(if (contains? e (cdr (cdr x)))
					(cons (car x) (remove-last e (cdr x)))
					(cons (car x) (cdr (cdr x))))]
			[else (cons (car x) (remove-last e (cdr x)))])))

(define contains?
	(lambda (x y)
		(if (equal? y '()) #f
			(if (equal? x (car y)) #t (contains? x (cdr y))))))




