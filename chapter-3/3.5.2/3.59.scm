(load "../../lib/stream")

;a

(define (integrate order coeff)
  (/ coeff (+ order 1)))

(define (integrate-series series)
  (stream-map integrate
              (integers-starting-from 0)
              series))
; testing
(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(display-stream (stream-take exp-series 5))
;=>
;1
;1
;1/2
;1/6
;1/24
;1/120
