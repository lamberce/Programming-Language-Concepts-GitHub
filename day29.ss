;;Day 29 class notes

(define-syntax flip
	(syntax-rules ()
		[(_ x) (begin (set! x (reverse x) x))]))
		
(