(load "./stream")

;  +-------+             +--------+
;  |map: f |---- ddy --->|integral|<-- dy0
;  +-------+             +--------+
;    ^   ^                   |
;    |   |                   |
;    |   +------- dy --------+
;    |                       |
;    |                       V
;    |                   +--------+
;    +------------ y ----|integral|<-- y0
;                        +--------+

(define integral delayed-integral)

(define (solve-2nd f dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (stream-map f dy y))
  y)

(define f (lambda (dy y) (+ (* 2 dy) (* 1 y))))

(define (main args)
  (display-stream
    (stream-take (solve-2nd f 0.01 1 0) 10)))
