(load "../../lib/stream")

(define (RC R C dt)
  (lambda (i v0)
    (define v
      (add-streams (scale-stream i R)
                   (integral (scale-stream i (/ C)) v0 dt)))
    v))

(define (main args)
  (define RC1 (RC 5 1 0.5))
  (display-stream
    (stream-take (RC1 ones 100) 10)))
