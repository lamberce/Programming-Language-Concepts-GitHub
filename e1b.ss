;;Chris Lambert
;;Exam #1b

;;Problem #5
(define rotate
	(lambda (x)
		(rotate-helper (cdr x) (car x))))
		
(define rotate-helper
	(lambda (x e)
		(if (null? x)
			(list e)
			(cons (car x) (rotate-helper (cdr x) e)))))
			
;;Problem #6
(define compose-with-list
	(lambda (x)
		(letrec ([func
					(lambda (x y)
						(if (null? x)
							y
							(func (cdr x) ((car x) y))))])
			(lambda (y) (func (reverse x) y)))))
			
;;Problem #7
(define compose
	(lambda x
		(compose-with-list x)))
						