(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))

; φ^2 = φ + 1
; 両辺をφで割ると
; φ = 1 + 1/φ

(define (phi x)
  (fixed-point (lambda (x) (+ 1 (/ 1 x)))
               1.0))

(define (main args)
  (print (phi 1000))
  ;=> 1.6180
  )
