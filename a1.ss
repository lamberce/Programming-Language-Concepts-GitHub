;;Chris Lambert
;;Assignment 1

;;Problem 1
(define Fahrenheit->Celsius
  (lambda (temperature) (/ (* (- temperature 32) 5) 9)))

;;Problem 2
(define interval-contains?
  (lambda (x y)
    (if (= (list-ref x 0) y) #t 
        (if (= (list-ref x 1) y) #t
            (if (> (list-ref x 0) y) #f 
                (if (> (list-ref x 1) y) #t #f))))))

;;Problem 3
(define interval-intersects?
  (lambda (x y)
    (if (interval-contains? x (list-ref y 0)) #t
        (if (interval-contains? x (list-ref y 1)) #t
            (if (interval-contains? y (list-ref x 0)) #t
                (if (interval-contains? y (list-ref x 1)) #t #f))))))

;;Problem 4
(define interval-union
  (lambda (x y)
    (if (interval-intersects? x y)
        (if (<= (list-ref x 0) (list-ref y 0))
            (if (<= (list-ref x 1) (list-ref y 1)) (list (list (list-ref x 0) (list-ref y 1)))
                (list (list (list-ref x 0) (list-ref x 1))))
            (if (<= (list-ref x 1) (list-ref y 1)) (list (list (list-ref y 0) (list-ref y 1)))
                (list (list (list-ref y 0) (list-ref x 1)))))
       (list x y))))

;;Problem 5
(define divisible-by-7?
  (lambda (x)
    (if (= (modulo x 7) 0) #t #f)))

;;Problem 6
(define ends-with-7? 
  (lambda (x)
    (if (= (modulo (- x 7) 10) 0) #t #f)))