;;Chris Lambert
;;Assignment #9

;;Problem #1
(define snlist-recur
	(lambda (b s-proc list-proc)
		(letrec ([func 
					(lambda (s)
						(cond
							[(null? s) b]
							[(not (pair? s)) (s-proc s)]
							[else (list-proc (func (car s)) (func (cdr s)))]))])
			(lambda (s) (func s)))))
	
;;Part a
(define sn-list-sum
	(lambda (s)
		((snlist-recur 0 (lambda (x) x) +) s)))
		
;;Part b
(define sn-list-map
	(lambda (proc s)
		((snlist-recur '() proc cons) s)))
		
;;Part c
(define sn-list-paren-count
	(lambda (s)
		((snlist-recur 2 (lambda (s) 0) (lambda (c s) (+ c s))) s)))
		
;;Part d
(define sn-list-reverse
	(lambda (s)
		((snlist-recur '() (lambda (s) s) (lambda (c s) (reverse (cons c (reverse s))))) s)))
		
;;Part e
(define sn-list-occur
	(lambda (e s)
		((snlist-recur 0 (lambda (s) (if (equal? s e) 1 0)) +) s)))
									
;;Part f
(define sn-list-depth
	(lambda (s)
		((snlist-recur 1 (lambda (s) 0) (lambda (c s) (max (+ c 1) s))) s)))

;;Problem #2
(define bt-recur
	(lambda (b b-proc m-proc)
		(letrec ([func
					(lambda (s)
						(cond
							[(null? s) b]
							[(not (pair? s)) (b-proc s)]
							[else (m-proc (func (car (cdr s))) (func (car s)) (func (car (cdr (cdr s)))))]))])
			(lambda (s) (func s)))))
			
;;Part a
(define bt-sum
	(lambda (s)
		((bt-recur 0 (lambda (x) (if (number? x) x 0)) +) s)))
		
;;Part b
(define bt-inorder
	(lambda (s)
		((bt-recur '() (lambda (x) (if (number? x) '() (list x))) append) s)))
			
							
						
						
						
						
						