(define (iterative-improve enough? improve)
  (define (iter guess)
    (let ((next (improve guess)))
      (if (enough? guess next)
        guess
        (iter next))))
  iter)

(define (average . args)
  (/ (apply + args) (length args)))

(define (sqrt x)
  (define (good-enough? guess x)
    (< (abs (- guess x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  ((iterative-improve good-enough? improve) 1.0))

(define tolerance 0.0001)

(define (fixed-point f guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  ((iterative-improve close-enough? f) 1.0))

(define (sqrt-2 x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))

(define (main args)
  (print (sqrt 2))
  (print (sqrt-2 2))
  )
