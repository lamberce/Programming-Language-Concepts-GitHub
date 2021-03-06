;; Test code for CSSE 304 Assignment 7

(define (test-member-n?)
    (let ([correct '(
		     (#f #t)
		     (#t #f)
		     (#f #t)
		     (#t #f)
		     (#f #t)
		     )]
          [answers 
            (list 
	     (list (member-n? 'a 1 '()) (member-n? 'a 1 '(a)))
	     (list (member-n? 'a 1 '(a)) (member-n? 'a 1 '(b)))
	     (list (member-n? 'a 1 '(b)) (member-n? 'a 2 '(b a b a b)))
	     (list (member-n? 'a 2 '(b a b a b)) (member-n? 'a 3 '(b a b a b)))
	     (list (member-n? 'a 3 '(b a b a b)) (member-n? 'b 3 '(b a b a b)))
	     )])
      (display-results correct answers equal?)))

(define (test-opposites-attract)
    (let ([correct '(
		     ()
		     ((a a))
		     ((a d) (b c) (c b) (d a))
		     ((a e) (b d) (c c) (d b) (e a))
		     )]
          [answers 
            (list 
	     (opposites-attract '())
	     (opposites-attract '(a))
	     (opposites-attract '(a b c d))
	     (opposites-attract '(a b c d e))
	     )])
      (display-results correct answers equal?)))



(define (test-symmetric?)
    (let ([correct '(
		     (#t #f)
		     (#t #f)
		     (#t #f)
		     (#t #f)
		     )]
          [answers 
            (list 
	     (list (symmetric? '()) (symmetric? '((a b))))
	     (list (symmetric? '((a a))) (symmetric? '((a b))))
	     (list (symmetric? '((a b) (c b) (b a) (c c) (b c))) (symmetric? '((a b))))
	    (list (symmetric? '((a a))) (symmetric? '((a b) (c b) (b d) (c c) (b c))))

	     )])
      (display-results correct answers equal?)))

(define (test-lower-triangular?)
    (let ([correct '(
		     (#t #f)
		     (#t #f)
		     (#t #f)
		     (#t #f)
		     )]
          [answers 
            (list 
	     (list (lower-triangular? '((2))) (lower-triangular? '((2 1) (0 3))))
	     (list (lower-triangular? '((2 0) (1 3))) (lower-triangular? '((2 1) (0 3))))
	     (list (lower-triangular? '((2 0 0) (3 4 0) (5 6 8))) (lower-triangular? '((2 1) (0 3))))
	     (list (lower-triangular? '((2))) (lower-triangular? '((2 0 0) (3 4 1) (5 6 8))))
	     )])
      (display-results correct answers equal?)))




;-----------------------------------------------

(define display-results
  (lambda (correct results test-procedure?)
     (display ": ")
     (pretty-print 
      (if (andmap test-procedure? correct results)
          'All-correct
          `(correct: ,correct yours: ,results)))))


(define sequal?-grading
  (lambda (l1 l2)
    (cond
     ((null? l1) (null? l2))
     ((null? l2) (null? l1))
     ((or (not (set?-grading l1))
          (not (set?-grading l2)))
      #f)
     ((member (car l1) l2) (sequal?-grading
                            (cdr l1)
                            (rember-grading
                             (car l1)
                             l2)))
     (else #f))))

(define set?-grading
  (lambda (s)
    (cond [(null? s) #t]
          [(not (list? s)) #f]
          [(member (car s) (cdr s)) #f]
          [else (set?-grading (cdr s))])))

(define rember-grading
  (lambda (a ls)
    (cond
     ((null? ls) ls)
     ((equal? a (car ls)) (cdr ls))
     (else (cons (car ls) (rember-grading a (cdr ls)))))))

(define set-equals? sequal?-grading)

(define find-edges  ; e know that this node is in the graph before we do the call
  (lambda (graph node)
    (let loop ([graph graph])
      (if (eq? (caar graph) node)
	  (cadar graph)
	  (loop (cdr graph))))))

;; Problem 8  graph?
(define set?  ;; Is this list a set?  If not, it is not a graph.
  (lambda (list)
    (if (null? list) ;; it's an empty set.
	#t
	(if (member (car list) (cdr list))
	    #f
	    (set? (cdr list))))))


(define graph?
  (lambda (obj)
    (and (list? obj)
	 (let ([syms (map car obj)])
	   (and (set? syms)
		(andmap symbol? syms)
		(andmap (lambda (x)
			  (andmap (lambda (y) (member y (remove (car x) syms)))
				  (cadr x)))
			obj))))))
    
(define graph-equal?
  (lambda (a b)
    (and
     (graph? a) 
     (graph? b)
     (let ([a-nodes (map car a)]
	   [b-nodes (map car b)])
       (and 
	(set-equals? a-nodes b-nodes)
	    ; Now  See if the edges from each node are equivalent in the two graphs.
	(let loop ([a-nodes a-nodes])
	  (if (null? a-nodes)
	      #t
	      (let ([a-edges (find-edges a (car a-nodes))]
		    [b-edges (find-edges b (car a-nodes))])
		(and (set-equals? a-edges b-edges)
		     (loop (cdr a-nodes)))))))))))

(define (test-graph-equal)
  (list
   (graph-equal? '((a (b)) (b (a))) '((b (a)) (a (b))))
   (graph-equal? '((a (b c d)) (b (a c d)) (c (a b d)) (d (a b c)))
		 '((b (a c d)) (c (a b d)) (a (b d c)) (d (b a c))))
   (graph-equal? '((a ())) '((a ())))
   (graph-equal? '((a (b c)) (b (a c)) (c (a b))) '((a (b c)) (b (a c)) (c (a b))))
   (graph-equal? '() '())
   ))



(define g test-graph-equal)
	   
	  
     



;You can run the tests individually, or run them all
;#by loading this file (and your solution) and typing (r)

(define (run-all)
  (display 'member-n? ) 
  (test-member-n? )
  (display 'opposites-attract) 
  (test-opposites-attract)
  (display 'symmetric?) 
  (test-symmetric?)
  (display 'lower-triangular?) 
  (test-lower-triangular?)    
) 
(define r run-all)