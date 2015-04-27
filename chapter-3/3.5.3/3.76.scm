(load "./3.74")

(define sense-data (stream 1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4))

(define (smooth s last-value)
  (cons-stream (average (stream-car s) last-value)
               (smooth (stream-cdr s)
                       (stream-car s))))

(define (make-zero-crossings input-stream last-value)
  (cons-stream
    (sign-change-detector (stream-car input-stream) last-value)
    (make-zero-crossings (stream-cdr input-stream)
                         (stream-car input-stream))))

(define zero-crossings
  (make-zero-crossings (smooth sense-data 0)
                       0))

(define (main args)
  (display-stream
    (stream-take (smooth sense-data 0) 11))
  ;=> 0.5 1.5 1.75 1.25 0.75 0.2 -1.05 -2.5 -2.5 -1.25 -0.15 1.6
  (newline)
  (display-stream
    (stream-take zero-crossings 11))
  ;=> 0 0 0 0 0 0 1 0 0 0 0 -1
  )
