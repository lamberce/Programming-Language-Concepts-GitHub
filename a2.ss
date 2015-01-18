;;Chris Lambert
;;Assignment #2

;;Problem #1
(define fact
  (lambda (x)
    (if (zero? x)	1
		(* x (fact (- x 1))))))
		
(define choose
	(lambda (x y)
		(/ (fact x) (* (fact (- x y)) (fact y)))))
		
;;Problem #2
(define make-range
	(lambda (m n)
		(if (<= n m) (list )
			(cons m (make-range (+ m 1) n)))))
			
;;Problem #3
(define set?
	(lambda (x)
		(if (equal? x '()) #t
			(if (contains? (car x) (cdr x)) #f
				(set? (cdr x))))))

(define contains?
	(lambda (x y)
		(if (equal? y '()) #f
			(if (equal? x (car y)) #t (contains? x (cdr y))))))
			
;;Problem #4
(define sum-of-squares
	(lambda (x)
		(if (equal? x '()) 0
			(+ (expt (car x) 2) (sum-of-squares (cdr x))))))

;;Problem #5
(define make-vec-from-points
	(lambda (p1 p2)
		(list (- (list-ref p2 0) (list-ref p1 0)) (- (list-ref p2 1) (list-ref p1 1))
			  (- (list-ref p2 2) (list-ref p1 2)))))
			  
;;Problem #6
(define dot-product
	(lambda (v1 v2)
		(+ (* (list-ref v1 0) (list-ref v2 0)) (* (list-ref v1 1) (list-ref v2 1))
		   (* (list-ref v1 2) (list-ref v2 2)))))
		   
;;Problem #7
(define vec-length
	(lambda (x)
		(sqrt (+ (expt (list-ref x 0) 2) (expt (list-ref x 1) 2) (expt (list-ref x 2) 2)))))

;;Problem #8
(define distance
	(lambda (p1 p2)
		(vec-length (make-vec-from-points p1 p2))))



