(define (f x y z)
  (let ((x (take (sort (list x y z) >) 2)))
    (+ (square (car x))
       (square (cadr x)))))

(print (f 3 9 1))
;=> 9 * 9 + 3 * 3 = 90
