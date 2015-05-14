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

; area-of-circle = πr^2
; => π = π * 1^2
(define (unit-circle-test x y)
  (<= (+ (square (- x 0))
         (square (- y 0)))
      (square 1)))

(define (estimate-integral P x1 x2 y1 y2)
  (define area-of-square
    (* (- x1 x2) (- y1 y2)))
  (define experiments
    (stream-map P
                (random-in-range-stream x1 x2)
                (random-in-range-stream y1 y2)))
  (scale-stream (monte-carlo experiments 0 0)
                area-of-square))

(define (main args)
  (print
    (inexact (stream-ref (estimate-integral unit-circle-test 5 -3 6 -2)
                         1000000)))
  ;=> 3.127740872259128
  )
