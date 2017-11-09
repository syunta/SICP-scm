(load "./1.37")

(define (tan-cf x k)
  (cont-frac (lambda (i)
               (if (= i 1)
                 x
                 (- (square x))))
             (lambda (i) (+ i (- i 1)))
             k))

(define (main args)
  (print (tan-cf 0.314 100))
  )

