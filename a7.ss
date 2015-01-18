;;Chris Lambert
;;Assignment #7

;;Problem #1
(define invert
	(lambda (x)
		(if (equal? x '())
			'()
			(cons (list (car (cdr (car x))) (car (car x))) (invert (cdr x))))))
			
;;Problem #2
(define vector-index
	(lambda (pred x)
		(vector-index-helper pred x 0)))
		
(define vector-index-helper
	(lambda (pred x loc)
		(if (= loc (vector-length x))
			#f
			(if (pred (vector-ref x loc)) 
				loc
				(vector-index-helper pred x (+ loc 1))))))
				
;;Problem #3
(define vector-append-list
	(lambda (v ls)
		(combineVecList (make-vector (+ (vector-length v) (length ls))) v ls 0 0 0)))
		
(define combineVecList
	(lambda (x v ls vref lsref k)
		(if (= (vector-length v) vref)
			(if (= (length ls) lsref)
				x
				(vector-set! x k (list-ref ls lsref)))
			(vector-set! x k (vector-ref v vref)))
		(if (= (vector-length v) vref)
			(if (= (length ls) lsref)
				x
				(combineVecList x v ls vref (+ lsref 1) (+ k 1)))
			(combineVecList x  v ls (+ vref 1) lsref (+ k 1)))))
			
;;Problem #4
(define getPivot
	(lambda (pred x)
		(cond
			[(equal? x '()) 'stop]
			[(equal? (cdr x) '()) 'stop]
			[(all-same? x (car x)) 'stop]
			[(pred (car x) (car (cdr x))) (getPivot pred (cdr x))]
			[else (car x)])))

(define split-lists 
	(lambda (pred piv x sl1 sl2)
		(if (equal? x '())
			(list sl1 sl2)
			(if (pred (car x) piv)
				(split-lists  pred piv (cdr x) (cons (car x) sl1) sl2)
				(split-lists  pred piv (cdr x) sl1 (cons (car x) sl2))))))
			
(define qsort
	(lambda (pred x)
		(let ([piv (getPivot pred x)])
			(if (equal? piv 'stop)
				x
				(let ([split (split-lists pred piv x '() '())])
					(append (qsort pred (car split)) (qsort pred (car (cdr split)))))))))
					
(define all-same?
	(lambda (x n)
		(if (equal? x '())
			#t
			(if (equal? (car x) n)
				(all-same? (cdr x) n)
				#f))))
					
;;Problem #5
(define connected?
	(lambda (g)
		(if (equal? (length (map car g)) 
			(length (testConnected g (cons (list-ref (list-ref g 0) 0) (list-ref (list-ref g 0) 1))  0 0 (length g))))
				#t
				#f)))

;;n is outer
;;k is inner
(define testConnected
	(lambda (g x n k s)
		(if (= n s)
			x
			(if (= k s)
				(testConnected g x (+ n 1) 0 s)
				(if (contains? (car (list-ref g k)) x)
					(testConnected g (union (list-ref (list-ref g k) 1) x) n (+ k 1) s)
					(testConnected g x n (+ k 1) s))))))

(define union
	(lambda (s1 s2)
		(create-domain-list (append s1 s2) '())))

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
			
;;Problem #6
(define reverse-it
	(lambda (x)
		(if (equal? x '())
			'()
			(append (reverse (cdr x)) (list (car x))))))
	
;;Problem #7
(define empty-BST
	(lambda ()
		'()))
	
(define empty-BST?
	(lambda (x)
		(if (equal? x '()) #t #f)))
		
(define BST-insert
	(lambda (x bst)
		(if (empty-BST? bst)
			(list x '() '())
			(cond
				[(= (car bst) x) bst]
				[(> (car bst) x) (list (car bst) (BST-insert x (car (cdr bst))) (car (cdr (cdr bst))))]
				[(< (car bst) x) (list (car bst) (car (cdr bst)) (BST-insert x (car (cdr (cdr bst)))))]))))
				
(define BST-inorder
	(lambda (bst)
		(if (empty-BST? bst)
			'()
			(append (BST-inorder (car (cdr bst))) (cons (car bst) (BST-inorder (car (cdr (cdr bst))))))))) 

(define BST?
	(lambda (bst)
		(if (not (node? bst))
			#f
			(if (empty-BST? bst)
				#t
				(if (and (check-left (BST-element bst) (BST-left bst)) 
					(check-right (BST-element bst) (BST-right bst))
					(BST? (BST-left bst)) (BST-right bst))
					#t
					#f)))))
			
(define check-left
	(lambda (n bst)
		(if (empty-BST? bst)
			#t
			(if (not (< (BST-element bst) n))
				#f
				(if (and (check-left n (BST-left bst)) (check-left n (BST-right bst)))
					#t
					#f)))))
				
(define check-right
	(lambda (n bst)
		(if (empty-BST? bst)
			#t
			(if (not (> (BST-element bst) n))
				#f
				(if (and (check-right n (BST-left bst)) (check-right n (BST-right bst)))
					#t
					#f)))))

(define node?
	(lambda (bst)
		(if (empty-BST? bst)
			#t
			(if (not (pair? bst))
				#f
				(if (not (= (length bst) 3))
					#f
					(if (number? (car bst))
						(if (and (node? (list-ref bst 1)) (node? (list-ref bst 2)))
							#t
							#f)
						#f))))))
							
(define BST-element
	(lambda (bst)
		(car bst)))
			
(define BST-left
	(lambda (bst)
		(car (cdr bst))))
			
(define BST-right
	(lambda (bst)
		(car (cdr (cdr bst)))))
			
(define BST-insert-nodes
	(lambda (bst x)
		(if (equal? '() x)
			bst
			(BST-insert-nodes (BST-insert (car x) bst) (cdr x)))))
			
(define BST-contains?
	(lambda (bst x)
		(if (empty-BST? bst)
			#f
			(cond
				[(= (car bst) x) #t]
				[(< (car bst) x) (BST-contains? (car (cdr (cdr bst))) x)]
				[else (BST-contains? (car (cdr bst)) x)]))))

;;Problem #8
(define map-by-position
	(lambda (fn-list arg-list)
		(map (lambda (x y) (x y)) fn-list arg-list)))
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
			