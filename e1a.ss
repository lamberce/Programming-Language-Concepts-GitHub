;;Chris Lambert
;;Exam #1a

;;Problem #1
(define member-n?
	(lambda (sym n ls)
		(if (= n (count-numb-sym sym ls)) #t #f)))
		
(define count-numb-sym
	(lambda (sym x)
		(if (null? x)
			0
			(if (equal? (car x) sym)
				(+ 1 (count-numb-sym sym (cdr x)))
				(count-numb-sym sym (cdr x))))))

;;Problem #2				
(define opposites-attract
	(lambda (x)
		(if (null? x)
			'()
			(reverse (create-domain-list (append (opposites-helper x 0 (- (length x) 1)) (mirror (reverse (opposites-helper x 0 (- (length x) 1))))) '())))))
			

;;n left
;;k right
(define opposites-helper
	(lambda (x n k)
		(if (= n k)
			(list (list (list-ref x n) (list-ref x n)))
			(if (< k n)
				'()
				(cons (list (list-ref x n) (list-ref x k))
					(opposites-helper x (+ n 1) (- k 1)))))))

(define mirror
	(lambda (x)
		(if (equal? x '())
			'()
			(let ([a (car (car x))] [b (car (cdr (car x)))])
				(cons (list b a) (mirror (cdr x)))))))
				
(define create-domain-list
	(lambda (x y)
		(cond
			[(equal? x '()) y]
			[(contains? (car x) y) (create-domain-list (cdr x) y)]
			[else (create-domain-list (cdr x) (cons (car x) y))])))

(define contains?
	(lambda (x y)
		(if (equal? y '()) #f
			(if (equal? x (car y)) #t (contains? x (cdr y))))))
	
;;Problem #3	
(define symmetric?
	(lambda (x)
		(if (= (length x) (length (create-domain-list (append x (mirror x)) '())))
			#t
			#f)))

;;Problem #4
(define lower-triangular?
	(lambda (x)
		(lower-helper x (- (length (car x)) 1) (- (length (car x)) 1))))
	
;;n is row
;;k is column	
(define lower-helper
	(lambda (m n k)
		(if (<= k 0)
			#t
			(if (< n 0)
				(lower-helper m (- k 1) (- k 1))
				(if (= n k)
					(lower-helper m (- n 1) k)
					(if (= 0 (matrix-ref m n k))
						(lower-helper m (- n 1) k)
						#f))))))		
			
(define matrix-ref
	(lambda (m row col)
		(list-ref (list-ref m row) col)))
	
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			