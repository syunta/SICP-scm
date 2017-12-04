(load "./2.33")

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ (* higher-terms x) this-coeff))
              0
              coefficient-sequence))

(define (main args)
  (print (horner-eval 2 (list 1 3 0 5 0 1)))
  )

