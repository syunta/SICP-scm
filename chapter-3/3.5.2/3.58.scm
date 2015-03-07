(load "../../lib/stream")

(define (expand num den radix)
  (cons-stream
    (quotient (* num radix) den)
    (expand (remainder (* num radix) den) den radix)))

(display-stream (stream-take (expand 1 7 10) 10))
;=> 1 4 2 8 5 7 1 4 2 8 5
(print (/ 1.0 7.0))
;=> 0.14285714285714285

(display-stream (stream-take (expand 3 8 10) 10))
;=> 3 7 5 0 0 0 0 0 0 0 0
(print (/ 3.0 8.0))
;=> 0.375
