; (n, k) = (n - 1, k - 1) + (n - 1, k)
; (n, 0) = (n, n) = 1

(define (pascal-triangle n k)
  (if (or (= k 0) (= n k))
    1
    (+ (pascal-triangle (- n 1) (- k 1))
       (pascal-triangle (- n 1) k))))

(define (main args)
  (print (pascal-triangle 0 0)) ;=> 1
  (print (pascal-triangle 1 0)) ;=> 1
  (print (pascal-triangle 2 1)) ;=> 2
  (print (pascal-triangle 3 1)) ;=> 3
  (print (pascal-triangle 3 2)) ;=> 3
  (print (pascal-triangle 4 2)) ;=> 6
  )
