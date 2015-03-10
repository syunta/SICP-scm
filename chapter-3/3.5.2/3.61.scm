(load "./3.60")

(define (invert-unit-series s)
  (cons-stream 1
               (mul-series (stream-cdr s)
                           (negate-stream (invert-unit-series s)))))
;test
(display-stream
  (stream-take (invert-unit-series (invert-unit-series exp-series)) 5))
;=>
;1
;1
;1/2
;1/6
;1/24
;1/120
(newline)
