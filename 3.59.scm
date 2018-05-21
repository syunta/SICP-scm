(load "./stream")

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
(newline)

;b
(define (negate-stream stream)
  (cons-stream (- (stream-car stream))
               (negate-stream (stream-cdr stream))))

(define cosine-series
  (cons-stream 1
               (negate-stream (integrate-series sine-series))))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

; testing
(display-stream (stream-take cosine-series 5))
;=>
;1
;0
;-1/2
;0
;1/24
;0
(newline)
(display-stream (stream-take sine-series 5))
;=>
;0
;1
;0
;-1/6
;0
;1/120
(newline)
