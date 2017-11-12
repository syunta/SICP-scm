(load "./1.43")

(define dx 0.0001)

(define (average . args)
  (/ (apply + args) (length args)))

(define (smooth f)
  (lambda (x)
    (average (f (- x dx))
             (f x)
             (f (+ x dx)))))

(define (n-smooth f n)
  ((repeated smooth n) f))

(define (main args)
  (print ((smooth square) 2))
  (print ((n-smooth square 2) 2))
  )
