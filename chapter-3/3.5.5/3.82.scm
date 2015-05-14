(load "../../lib/stream")
(use srfi-27)

(define (random-real-stream)
  (cons-stream (random-real)
               (random-real-stream)))

(define (random-in-range-stream low high)
  (let ((range (- high low)))
    (stream-map (lambda (s) (+ low s))
                (scale-stream (random-real-stream)
                              range))))

(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
      (/ passed (+ passed failed))
      (monte-carlo
        (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
    (next (+ passed 1) failed)
    (next passed (+ failed 1))))
