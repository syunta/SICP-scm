(load "./1.17")

(define (mul a b)
  (mul-iter a b 0))

(define (mul-iter a b p)
  (cond ((= b 0) p)
        ((even? b) (mul-iter (double a) (halve b) p))
        (else (mul-iter a (- b 1) (+ p a)))))

(define (main args)
  (print (mul 3 1))
  (print (mul 3 2))
  (print (mul 3 3))
  (print (mul 3 4))
  )
