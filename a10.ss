;;Chris Lambert
;;Assignment #10

;;Problem #1
(define make-slist-leaf-iterator
	(lambda (x)
		(let ([stack (make-stack)])
			(stack 'push x)
			(letrec ([func 
						(lambda ()
							(if	(stack 'empty?)
								#f
								(let ([current (stack 'pop)])
									(if (null? current)
										(func)
										(if (symbol? current)
											current
											(begin  (stack 'push (cdr current))
													(stack 'push (car current)) 
												    (func)))))))])
				(lambda () (func))))))
	
		
(define make-stack
	(lambda ()
		(let ([stk '()])
			(lambda (msg  . args ) 
				(case msg
					[(empty?) (null? stk)]
					[(push)   (set! stk (cons (car args) stk))]
					[(pop)    (let ([top (car stk)])
						(set! stk (cdr stk))
						top)]
					[else (errorf 'stack "illegal message to stack object: ~a" msg)])))))
		
;;Problem #2
(define free-vars
	(lambda (x)
		(free-vars-helper x '() '())))
		

(define free-vars-helper
	(lambda (func free bound)
		(cond
			[(symbol? func) (if (contains? func bound)
								free
								(cons func free))]
			[(equal? (car func) 'lambda) (free-vars-helper (caddr func) free (append bound (cadr func)))]
			[else (create-domain-list (apply append (map (lambda (y) (free-vars-helper y free bound)) func)) '())])))

(define create-domain-list
	(lambda (x y)
		(cond
			[(equal? x '()) y]
			[(contains? (car x) y) (create-domain-list (cdr x) y)]
			[else (create-domain-list (cdr x) (cons (car x) y))])))
		
(define bound-vars
	(lambda (x)
		(bound-vars-helper x '() '())))

(define bound-vars-helper
	(lambda (func usedbound bound)
		(cond
			[(symbol? func) (if (contains? func bound)
								(cons func usedbound)
								usedbound)]					
			[(equal? (car func) 'lambda) (bound-vars-helper (caddr func) usedbound (append bound (cadr func)))] 
			[else (create-domain-list (apply append (map (lambda (y) (bound-vars-helper y usedbound bound)) func)) '())])))
					
(define contains?
	(lambda (x y)
		(if (equal? y '()) #f
			(if (equal? x (car y)) #t (contains? x (cdr y))))))
		
;;Problem #3
;;Part a
(define occurs-free?
	(lambda (sym x)
		(occurs-free-helper? sym x '())))

(define occurs-free-helper?
	(lambda (sym func bound)
		(cond
			[(symbol? func) (if (and (equal? func sym) (not (contains? func bound))) #t #f)]
			[(equal? (car func) 'lambda) (occurs-free-helper? sym (caddr func) (append (cadr func) bound))]
			[(equal? (car func) 'if) (or (occurs-free-helper? sym (cadr func) bound) (occurs-free-helper? sym (caddr func) bound))]
			[(equal? (car func) 'let) (or (ormap (lambda (y) (occurs-free-helper? sym y bound)) (map cadr (cadr func))) 
										  (occurs-free-helper? sym (caddr func) (append bound (map car (cadr func)))))]
			[(equal? (car func) 'let*) (or (true-before-free? sym (map car (cadr func)) (map (lambda (y) (occurs-free-helper? sym y bound)) (map cadr (cadr func)))) 
										  (occurs-free-helper? sym (caddr func) (append bound (map car (cadr func)))))]
			[(equal? (car func) 'set!) #f]
			[else (ormap (lambda (y) (occurs-free-helper? sym y bound)) func)])))
			
(define true-before-free?
	(lambda (sym bound checking)
		(if (null? bound)
			(ormap (lambda (x) x) checking)
			(if (equal? sym (car bound))
				#f
				(if (car checking)
					#t
					(true-before-free? sym (cdr bound) (cdr checking)))))))

;;Part b		
(define occurs-bound?
	(lambda (sym x)
		(occurs-bound-helper? sym x '())))
		
(define occurs-bound-helper?
	(lambda (sym func bound)
		(cond
			[(symbol? func) (if (and (equal? func sym) (contains? func bound)) #t #f)]
			[(equal? (car func) 'lambda) (occurs-bound-helper? sym (caddr func) (append (cadr func) bound))] 
			[(equal? (car func) 'if) (or (occurs-bound-helper? sym (cadr func) bound) (occurs-bound-helper? sym (caddr func) bound))]
			[(equal? (car func) 'let) (or (ormap (lambda (y) (occurs-bound-helper? sym y bound)) (map cadr (cadr func))) 
										  (occurs-bound-helper? sym (caddr func) (append bound (map car (cadr func)))))]
			[(equal? (car func) 'let*) (or (true-before-bound? sym (map car (cadr func)) (map (lambda (y) (occurs-free-helper? sym y bound)) (map cadr (cadr func)))) 
										  (occurs-bound-helper? sym (caddr func) (append bound (map car (cadr func)))))]
			[(equal? (car func) 'set!) (if (equal? sym (cadr func)) #t #f)]
			[else (ormap (lambda (y) (occurs-bound-helper? sym y bound)) func)])))
		
(define true-before-bound?
	(lambda (sym bound checking)
		(if (null? bound)
			#f
			(if (equal? sym (car bound))
				(ormap (lambda (x) x) (cdr checking))
				(true-before-bound? sym (cdr bound) (cdr checking))))))
		
		
		