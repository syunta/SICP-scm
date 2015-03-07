(load "../../lib/stream")

(define (partial-sums s)
  (cons-stream (stream-car s)
               (add-streams (partial-sums s)
                            (stream-cdr s))))

(display-stream (stream-take (partial-sums integers) 5))
;=>
;1
;3
;6
;10
;15
;21
