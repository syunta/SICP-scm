(load "../../lib/stream")

(define integral delayed-integral)

(define (solve-2nd a b dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (add-streams (scale-stream dy a)
                           (scale-stream y b)))
  y)

(define (main args)
  (display-stream
    (stream-take (solve-2nd 2 1 0.01 1 0) 10)))
