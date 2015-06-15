(load "../../lib/stream")

(define (rand-update x)
  (let ((a 1664525) (b 1013904223) (c (expt 2 32)))
    (modulo (+ (* a x) b) c)))

(define random-init 10)

(define random-numbers
  (cons-stream random-init
               (stream-map rand-update random-numbers)))

(define random-real-stream
  (scale-stream random-numbers (/ (expt 2.0 32))))

(define (random-in-range-clossings low1 high1 low2 high2)
  (define (go rand-stream low1 high1 low2 high2)
    (let ((range (- high1 low1)))
      (cons-stream (+ low1 (* range (stream-car rand-stream)))
                   (go (stream-cdr rand-stream) low2 high2 low1 high1))))
  (go random-real-stream low1 high1 low2 high2))

(define (map-successive-pairs f s)
  (cons-stream
    (f (stream-car s) (stream-car (stream-cdr s)))
    (map-successive-pairs f (stream-cdr (stream-cdr s)))))

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
    (map-successive-pairs P
                          (random-in-range-clossings x2 x1 y2 y1)))
  (scale-stream (monte-carlo experiments 0 0)
                area-of-square))

(define (main args)
  (print
    (inexact (stream-ref (estimate-integral unit-circle-test 5 -3 6 -2)
                         1000000)))
  ;=> 3.1446368553631445
  )
