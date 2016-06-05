(load "./1.37")

(define (e k)
  (cont-frac (lambda (i) 1.0)
             (lambda (i)
               (let ((n (+ i 1.0)))
                 (if (zero? (remainder n 3))
                   (* 2 (quotient n 3))
                   1.0)))
             k))

(define (main args)
  (print (e 100))
  ;=> e=2.718, e - 2 = 0.718
  )
