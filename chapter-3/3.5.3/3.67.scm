(load "../../lib/stream")

(define (pairs s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave
      (stream-map (lambda (x) (list x (stream-car t)))
                  (stream-cdr s))
      (interleave
        (stream-map (lambda (x) (list (stream-car s) x))
                    (stream-cdr t))
        (pairs (stream-cdr s) (stream-cdr t))))))

(define (main args)
  (display-stream (stream-take (pairs integers integers) 20)))
;=>
;(1 1)
;(2 1)
;(1 2)
;(3 1)
;(2 2)
;(4 1)
;(1 3)
;(5 1)
;(3 2)
;(6 1)
;(1 4)
;(7 1)
;(2 3)
;(8 1)
;(1 5)
;(9 1)
;(4 2)
;(10 1)
;(1 6)
;(11 1)
;(3 3)
