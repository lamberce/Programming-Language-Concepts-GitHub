;Graham Fuller & Chris Lambert
;Assignment 12

;Problem 1
(define zero
	(lambda ()
		'()))
		
(define BASE)
		
		
(define pred
	(lambda (x)
		(shorten (helpspred x 1))))
		
(define succ
	(lambda (x)
		(shorten (helpssucc x 1))))
			
(define helpspred
	(lambda (x carry)
		(cond
			[(= 0 carry) x]
			[(equal? x '()) 
				(if carry '(1) '())]
			[(= (car x) 0) (cons (- BASE 1) (helpspred (cdr x) 1))]
			[else (cons (- (car x) carry) (helpspred (cdr x) 0))])))
			
(define helpssucc
	(lambda (x carry)
		(cond
			[(= carry 0) x]
			[(equal? x '()) 
				(if (= carry 1) '(1) '())]
			[(= (car x) (- BASE 1)) (cons 0 (helpssucc (cdr x) 1))]
			[else (cons (+ (car x) carry) (helpssucc (cdr x) 0))])))
			
(define int->bignum
	(lambda (x)
		(if (= x 0)
			'()
			(helper2 x 0))))
		
(define helper2
	(lambda (x powe)
		(cond
		[(= x 0) 
			(if (= powe -1)'() (append (helper2 x (- powe 1)) '(0)))]
		[(<= 1 (/ x (expt BASE (+ 1 powe)))) (helper2 x (+ powe 1))]
		[(> 1 (/ x (expt BASE powe))) (append (helper2 x (- powe 1)) '(0))]
		[else (append (helper2 (remainder x (expt BASE powe)) (- powe 1)) (list (quotient x (expt BASE powe))))])))
		
(define bignum->int
	(lambda (x)
		(helpfelp x 0)))
		
(define helpfelp
	(lambda (x pow)
		(cond
		[(equal? x '()) 0]
		[else (+ (* (car x) (expt BASE pow)) (helpfelp (cdr x) (+ pow 1)))])))
		
(define helper3
	(lambda (x powe)
		(cond
		[(equal? x '()) 0]
		[else (+ (helper3 (cdr x) (+ powe 1)) (* (car x) (expt BASE powe)))])))
		
(define shorten
	(lambda (x)	
		(cond
		[(equal? x '(0)) '()] 
		[(equal? (last x) 0) (shorten (all-but-last x))]
		[else x])))
		
(define last
	(lambda (x)
		(if (equal? (cdr x) '())
			(car x)
			(last (cdr x)))))
			
(define all-but-last
	(lambda (x)
		(if (= (length x) 1)
			'()
			(cons (car x) (all-but-last (cdr x))))))
		
(define plus
	(lambda (x y)
		(if (equal? x '())
			y
			(succ (plus (pred x) y)))))
			
(define multiply
	(lambda (x y)
		(helpineedsomebody '() x y)))
		
(define helpineedsomebody
	(lambda (sum x y)
		(if (equal? y '())
			sum
			(helpineedsomebody (plus sum x) x (pred y)))))
			
(define factorial
	(lambda (x)
			(if (equal? x '())
				'(1)
				(helpsy '(1) x))))
		
(define helpsy
	(lambda (pro mul)
		(if (equal? mul '(1))
			pro
			(helpsy (multiply pro mul) (pred mul)))))

;;Problem #3
(define dt?
	(lambda (dt)
		(if (equal? (car dt) 'one)
			#t
			(if (not (equal? (car dt) 'diff))
				#f
				(if (not (equal? (length dt) 3))
					#f
					(if (and (diff-tree? (cadr dt)) (diff-tree? (caddr dt)))
						#t
						#t))))))

(define dt-negate
	(lambda (dt)
		(integer->dt (* -1 (dt->integer dt)))))
		
(define dt+
	(lambda (dt1 dt2)
		(integer->dt (+ (dt->integer dt1) (dt->integer dt2)))))
	
(define dt-
	(lambda (dt1 dt2)
		(integer->dt (- (dt->integer dt1) (dt->integer dt2)))))
	
(define dt=
	(lambda (dt1 dt2) 
		(if (equal? (dt->integer dt1) (dt->integer dt2)) #t #f)))
	
(define dt->integer
	(lambda (dt)
		(if (equal? (car dt) 'one)
			1
			(- (dt->integer (cadr dt)) (dt->integer (caddr dt))))))
		
(define integer->dt
	(lambda (n)
		(cond
			[(= n 0) (list 'diff '(one) '(one))]
			[(= n 1) (list 'one)]
			[(< n 0) (list 'diff (list 'diff '(one) '(one)) (integer->dt (* -1 n)))]
			[(> n 0) (list 'diff (list 'diff '(one) (integer->dt (+ (* n -1) 1))) (list 'diff '(one) '(one)))])))
			

;;Problem #4
(define-datatype bintree bintree? 
	(leaf-node 
		(num integer?)) 
	(interior-node 
		(key symbol?) 
		(left-tree bintree?) 
		(right-tree bintree?))) 

(define bintree-to-list
	(lambda (t)
		(cases bintree t
			[leaf-node (num) (list 'leaf-node num)]
			[interior-node (key left-tree right-tree) (list 'interior-node key left-tree right-tree)])))
		
;;Problem #5 
(define max-interior
	(lambda (t)
		(car (max-interior-helper t))))
		
(define max-interior-helper
	(lambda (t)
		(cases bintree t
			[leaf-node (num) '(())]
			[interior-node (key left-tree right-tree)
							(cond
								[(and (equal? 'leaf-node (car left-tree)) (equal? 'leaf-node (car right-tree)))
									(list key (+ (cadr left-tree) (cadr right-tree)) (+ (cadr left-tree) (cadr right-tree)))]
								[(equal? 'leaf-node (car left-tree))
									(let ([temp (max-interior-helper right-tree)]) 
										(if (< (cadr left-tree) 0)
											(list (car temp) (cadr temp) (+ (cadr left-tree) (caddr temp)))
											(list key (+ (cadr left-tree) (caddr temp)) (+ (cadr left-tree) (caddr temp)))))]
								[(equal? 'leaf-node (car right-tree))
									(let ([temp (max-interior-helper left-tree)])
										(if (<= (cadr right-tree) 0)
											(list (car temp) (cadr temp) (+ (cadr right-tree) (caddr temp)))
											(list key (+ (cadr right-tree) (caddr temp)) (+ (cadr right-tree) (caddr temp)))))]			
								[else
									(let ([temp1 (max-interior-helper left-tree)] 
										  [temp2 (max-interior-helper right-tree)])
											(if (>= (cadr temp1) (cadr temp2))
												(if (>= (cadr temp1) (+ (caddr temp1) (caddr temp2)))
													temp1
													(list key (+ (caddr temp1) (caddr temp2)) (+ (caddr temp1) (caddr temp2))))
												(if (>= (+ (caddr temp1) (caddr temp2)) (cadr temp2))
													(list key (+ (caddr temp1) (caddr temp2)) (+ (caddr temp1) (caddr temp2)))
													temp2)))])])))
										  
										  
										  
										  
										  
										  
										  
										  
										  
										  
										  
										  
										  
										  
										  
										  
									
									
									
									
									
									
							
										
										
										
										
										
										
										
										
										