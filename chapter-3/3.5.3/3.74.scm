(load "../../lib/stream")

(define (sign-change-detector v1 v2)
  (cond ((and (< v1 0) (<= 0 v2)) 1)
        ((and (<= 0 v1) (<= 0 v2)) 0)
        ((and (< v1 0) (< v2 0)) 0)
        (else -1)))

(define (make-zero-crossings input-stream last-value)
  (cons-stream
    (sign-change-detector (stream-car input-stream) last-value)
    (make-zero-crossings (stream-cdr input-stream)
                         (stream-car input-stream))))

(define sense-data (stream 1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4))

;(define zero-crossings (make-zero-crossings sense-data 0))

(define zero-crossings
  (stream-map sign-change-detector
              sense-data
              (cons-stream 0 sense-data)))

(define (main args)
  (display-stream
    (stream-take zero-crossings 11))
  ;=> 0 0 0 0 0 1 0 0 0 0 -1 0
  )
