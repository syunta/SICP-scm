(load "./stream")

(define (partial-sums s)
  (define sum
    (cons-stream (stream-car s)
                 (add-streams sum
                              (stream-cdr s))))
  sum)

(display-stream (stream-take (partial-sums integers) 5))
;=>
;1
;3
;6
;10
;15
;21
