(use srfi-27)

(define (random x)
  (* x (random-real)))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
            (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (circle-test x y)
  (>= (square 1)
      (+ (square (- x 0))
         (square (- y 0)))))

(define (estimate-integral P x1 x2 y1 y2 n)
  (define (area-of-square x1 x2 y1 y2)
    (* (- x1 x2) (- y1 y2)))
  (define (experiment)
    (P (random-in-range x2 x1) (random-in-range y2 y1)))
  (* (area-of-square x1 x2 y1 y2)
     (monte-carlo n experiment)))

(print (estimate-integral circle-test 5 -3 6 -2 10000000.0))
;=> 3.1366336
