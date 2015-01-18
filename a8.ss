;;Chris Lambert
;;Assignment #8

;;Problem #1
;;Part a
(define slist-map
	(lambda (proc x)
		(if (null? x)
			'()
			(if (not (pair? x))
				(proc x)
				(map ((curry2 slist-map) proc) x)))))
			
(define curry2
	(lambda (x)
		(letrec ([func1
					(lambda (x y)
						(letrec ([func2
									(lambda (x y z)
										(x y z))])		
							(lambda (z) (func2 x y z))))])
			(lambda (y) (func1 x y)))))
			
;;Part b
(define slist-reverse
	(lambda (x)
		(if (null? x)
			'()
			(if (not (pair? x))
				x
				(map slist-reverse (reverse x))))))
		
;;Part c
(define slist-paren-count
	(lambda (x)
		(if (null? x)
			2
			(if (not (pair? x))
				0
				(+ 2 (apply + (map slist-paren-count x)))))))
		
(define times-two
	(lambda (x)
		(* x 2)))
		
;;Part d
(define slist-depth
	(lambda (x)
		(if (null? x)
			1
			(if (not (pair? x))
				0
				(+ 1 (apply max (map slist-depth x)))))))
			
;;Part e
(define slist-symbols-at-depth
	(lambda (x d)
		(if (null? x)
			'()
			(if (not (pair? x))
				(if (= d 0)
					(list x)
					'())
				(if (not (= d 0))
					(apply append (map slist-symbols-at-depth x (create-list-of-length (- d 1) (length x))))
					'())))))
					
					
;;Problem #2
(define subst-leftmost
	(lambda (new old x proc)
		(if (null? x)
			'()
			(if (not (pair? (car x)))
				(if (proc (car x) old) 
					(cons new (cdr x))
					(cons (car x) (subst-leftmost new old (cdr x) proc)))
				(if (slist-contains? (car x) old proc)
					(cons (subst-leftmost new old (car x) proc) (cdr x))
					(cons (car x) (subst-leftmost new old (cdr x) proc)))))))
					
(define slist-contains?
	(lambda (x e proc)
		(if (null? x)
			#f
			(if (not (pair? x))
				(if (proc x e)
					#t
					#f)
				(ormap slist-contains? x (create-list-of-length e (length x)) (create-list-of-length proc (length x)))))))

(define create-list-of-length
	(lambda (e n)
		(if (= n 0)
			'()
			(cons e (create-list-of-length e (- n 1))))))
				
;;Problem #3
(define bt-leaf-sum
	(lambda (x)
		(if (not (pair? x))
			x
			(if (number? (car x))
				(+ (car x) (bt-leaf-sum (car (cdr x))) (bt-leaf-sum (car (cdr (cdr x)))))
				(+ (bt-leaf-sum (car (cdr x))) (bt-leaf-sum (car (cdr (cdr x)))))))))
			
(define bt-inorder-list
	(lambda (x)
		(if (not (pair? x))
			'()
			(append (bt-inorder-list (car (cdr x))) (list (car x)) 
				(bt-inorder-list (car (cdr (cdr x))))))))
				
(define bt-max
	(lambda (x)
		(if (not (pair? x))
			x
			(if (number? (car x))
				(max (car x) (bt-max (car (cdr x))) (bt-max (car (cdr (cdr x)))))
				(max (bt-max (car (cdr x))) (bt-max (car (cdr (cdr x)))))))))
				
(define bt-max-interior
	(lambda (x)
		(letrec ([func
					(lambda (x)
						(cond
							[(and (number? (car (cdr x))) (number? (car (cdr (cdr x))))) 
								(list (+ (car (cdr x)) (car (cdr (cdr x)))) (car x))]
							[(number? (car (cdr x)))
								(if (< (car (cdr x)) 0)
									(cons (+ (car (cdr x)) (car (func (car (cdr (cdr x))))))
										  (cdr (func (car (cdr (cdr x))))))
									(cons (+ (car (cdr x)) (car (func (car (cdr (cdr x))))))
										  (list (car x))))]
							[(number? (car (cdr (cdr x))))
								(if (<= (car (cdr (cdr x))) 0)
									(cons (+ (car (cdr (cdr x))) (car (func (car (cdr x)))))
										  (cdr (func (car (cdr x)))))
									(cons (+ (car (cdr (cdr x))) (car (func (car (cdr x)))))
										  (list (car x))))]
							[else (if (< (car (func (car (cdr x)))) (car (func (car (cdr (cdr x))))))
									  (func (car (cdr (cdr x))))
									  (func (car (cdr x))))]))])		
			(car (cdr (func x))))))
			
			
			
			
			
			
			
			
			
			
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
							