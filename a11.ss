;;Chris Lambert
;;Assignment #11

;;Problem #1
(define lexical-address
	(lambda (func)
		(lexical-address-helper func 0 '())))
		
(define lexical-address-helper
	(lambda (func depth bound)
		(cond
			[(symbol? func) (if (slist-contains? bound func equal?) (cons ': (get-depth-difference func bound depth)) (list ': 'free func))]
			[(equal? (car func) 'lambda) (list (car func) (cadr func) (lexical-address-helper (caddr func) (+ depth 1) (append (create-bound-list (cadr func) depth 0) bound)))]
			[(equal? (car func) 'if) (list (car func) (lexical-address-helper (cadr func) depth bound) (lexical-address-helper (caddr func) depth bound) (lexical-address-helper (cadddr func) depth bound))]
			[else (map (lambda (x) (lexical-address-helper x depth bound)) func)])))

(define create-bound-list
	(lambda (func depth pos)
		(if (null? func)
			'()
			(cons (list (car func) depth pos) (create-bound-list (cdr func) depth (+ pos 1)))))) 
			
(define slist-contains?
	(lambda (x e proc)
		(if (null? x)
			#f
			(if (not (pair? x))
				(if (proc x e)
					#t
					#f)
				(ormap (lambda (y) (slist-contains? y  e proc)) x)))))
			
(define get-depth-difference
	(lambda (var bound depth)
		(if (null? bound) 
			#f
			(if (equal? var (caar bound))
				(list (- depth (+ (cadar bound) 1)) (caddar bound))
				(get-depth-difference var (cdr bound) depth)))))			

;;Problem #2
(define un-lexical-address
	(lambda (func)
		(un-lexical-address-helper func 0 '())))
	
(define un-lexical-address-helper
	(lambda (func depth bound)
		(cond
			[(equal? (car func) ':) (if (equal? (cadr func) 'free) (caddr func) (get-var (cadr func) depth (caddr func) bound))]
			[(equal? (car func) 'lambda) (list (car func) (cadr func) (un-lexical-address-helper (caddr func) (+ depth 1) (append (create-bound-list (cadr func) depth 0) bound)))]
			[(equal? (car func) 'if) (list (car func) (un-lexical-address-helper (cadr func) depth bound) (un-lexical-address-helper (caddr func) depth bound) (un-lexical-address-helper (cadddr func) depth bound))]
			[else (map (lambda (x) (un-lexical-address-helper x depth bound)) func)])))
			
(define get-var
	(lambda (depth current-depth pos bound)
		(if (null? bound)
			#f
			(if (and (equal? (- (- current-depth depth) 1) (cadar bound)) (equal? pos (caddar bound)))
				(caar bound)
				(get-var depth current-depth pos (cdr bound))))))
			
			

;;Problem #3
;;Part a
(define-syntax my-let
	(syntax-rules ()
		[(_ ((x v) ...) e1 e2 ...)
		 ((lambda (x ...) e1 e2 ...) v ...)]
		 [(_ name ([x k] ...) e1)
			(letrec ([name (lambda (x ...) e1)]) (name k ...))]))
								
;;Part b
(define-syntax my-or
	(syntax-rules ()
		((_) #f)
		((_ e1) e1)
		((_ e1 e2 ...)
		 (let ((temp e1))
			(if temp
				temp
				(my-or e2 ...))))))

;;Part c
(define-syntax +=
	(syntax-rules ()
		((_ x e2) (begin (set! x (+ x e2)) x))))
		
;;Part d
(define-syntax return-first
	(syntax-rules ()
		((_ e1 e2 ...) (let ([temp e1]) (begin e2 ... temp)))))