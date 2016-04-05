(define (f x y z)
  (let ((x (take (sort (list x y z) >) 2)))
    (+ (square (car x))
       (square (cadr x)))))

(define (f2 x y z)
  (let ((f (lambda (a b)
             (+ (square a)
                (square b)))))
  (cond ((< x y z) (f y z))
        ((< y x z) (f x z))
        (else (f x y)))))

(define (main args)
  (print (f 1 2 3))
  (print (f2 1 2 3))
  ;=> 2 * 2 + 3 * 3 = 13
  (print (f 4 2 5))
  (print (f2 4 2 5))
  ;=> 4 * 4 + 5 * 5 = 41
  (print (f 3 9 1))
  (print (f2 3 9 1))
  ;=> 9 * 9 + 3 * 3 = 90
  )
