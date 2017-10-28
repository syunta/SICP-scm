(define (double a) (+ a a))

(define (halve a) (/ a 2))

(define count 0)

(define (mul a b)
  ;(set! count (+ count 1))
  (cond ((= b 0) 0)
        ((even? b) (double (mul a (halve b))))
        (else (+ a (mul a (- b 1))))))

(define (main args)
  (print (mul 3 200))
  ;(print count)
  )
