(define (improve guess x)
  (/ (+ (/ x (square guess))
        (* 2 guess))
     3))

(define (good-enough? g1 g2)
  (< (abs (- g1 g2)) 0.001))

(define (cbrt-iter g1 g2 x)
  (if (good-enough? g1 g2)
    g1
    (cbrt-iter (improve g1 x)
               (improve g2 x)
               x)))

(define (cbrt x)
  (cbrt-iter 1.0 1000.0 x))

(define (main args)
  (print (cbrt 1000))
  ;=> 10.0 ..
  (print (cbrt 125))
  ;=> 5.0
  )
