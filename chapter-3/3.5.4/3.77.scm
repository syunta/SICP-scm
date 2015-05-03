(load "../../lib/stream")

(define (integral integrand initial-value dt)
  (cons-stream initial-value
               (if (stream-null? integrand)
                 the-empty-stream
                 (integral (stream-cdr integrand)
                           (+ (* dt (stream-car integrand))
                              initial-value)
                           dt))))

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

(define (main args)
  (print
    (stream-ref (solve (lambda (y) y) 1 0.001) 1000))
  ;2.716923932235896
  )
