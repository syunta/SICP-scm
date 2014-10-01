(use srfi-27)

(define (random x)
  (/ (random-integer (* x 10000)) 10000))

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
  (>= (expt 1 2)
      (+ (expt (- x 0) 2)
         (expt (- y 0) 2))))

(define (estimate-integral P x1 x2 y1 y2 n)
  (define (area-of-square x1 x2 y1 y2)
    (* (- x1 x2) (- y1 y2)))
  (define (experiment)
    (P (random-in-range x2 x1) (random-in-range x2 x1)))
  (* (area-of-square x1 x2 y1 y2)
     (monte-carlo n experiment)))

(print (estimate-integral circle-test 5 (- 3) 6 (- 2) 1000000.0))