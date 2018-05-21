(load "./stream")

(define factorials
  (cons-stream 1
               (mul-streams (stream-cdr integers)
                            factorials)))

(display-stream (stream-take factorials 5))
;=>
;1
;2
;6
;24
;120
;720
