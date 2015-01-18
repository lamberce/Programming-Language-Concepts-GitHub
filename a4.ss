;;Chris Lambert
;;Assignment 4

;;Problem #1
(define union
	(lambda (s1 s2)
		(create-domain-list (mergelists s1 s2) '())))

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
			
(define mergelists
	(lambda (l1 l2)
		(if (equal? l2 '()) 
			l1
			(mergelists (cons (car l2) l1) (cdr l2)))))
			
;;Problem #2
(define reflexive?
	(lambda (x)
		(if (= (numbT (map pairs x)) (size (domain x)))
			#t
			#f)))

(define pairs
	(lambda (x)
		(if (equal? (car x) (car (cdr x)))
			#t
			#f)))
		
(define numbT
	(lambda (x)
		(if (equal? x '())
			0
			(if (equal? (car x) #t) 
				(+ 1 (numbT (cdr x)))  
				(numbT (cdr x))))))
				
(define domain
	(lambda (x)
		(let ([x (map first x)])
			(create-domain-list x '()))))

(define first
	(lambda (x)
		(list (list-ref x 0))))
		
(define size
	(lambda (x)
		(if (equal? x '())
			0
			(+ 1 (size (cdr x))))))
		
;;Problem #3
(define matrix-ref
	(lambda (m row col)
		(list-ref (list-ref m row) col)))
		
;;Problem #4
(define matrix?
	(lambda (x)
		(cond
			[(not (or (pair? x) (null? x))) #f]
			[(= (numbT (map checkSubList? x)) (size x)) 
				(if (= 1 (size (union (map size x) '()))) #t #f)]
			[else #f])))
			
(define checkSubList?
	(lambda (x)
		(if (or (pair? x)) #t #f)))
			

(define size
	(lambda (x)
		(if (equal? x '())
			0
			(+ 1 (size (cdr x))))))
			
;;Problem #5
(define matrix-transpose
	(lambda (x)
		(colTrans x 0 0)))

(define colTrans
	(lambda (x col row)
		(if (= (column-size x) col)
			'()
			(cons (rowTrans x col 0) (colTrans x (+ 1 col) 0)))))

(define rowTrans
	(lambda (x col row)
		(if (= row (row-size x))
			'()
			(cons (matrix-ref x row col) (rowTrans x col (+ row 1))))))
			
(define column-size
	(lambda (x)
		(size (car x))))
		
(define row-size
	(lambda (x)
		(size x)))
		
;;Problem #6
(define last
	(lambda (x)
		(if (equal? (cdr x) '())
			(car x)
			(last (cdr x)))))

;;Problem #7
(define all-but-last
	(lambda (x)
		(if (equal? (cdr x) '()) 
			'()
			(cons (car x) (all-but-last (cdr x))))))
				
				
				
				
				
				
				
				
				
				
				
				
				
				
				
				
				