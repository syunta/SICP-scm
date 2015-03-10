(load "./3.61")

(define (invert-series stream)
  (scale-stream (cons-stream (stream-car stream)
                             (mul-series (stream-cdr stream)
                                         (negate-stream (invert-series stream))))
                (/ (stream-car stream))))

(define (div-series s1 s2)
  (if (zero? (stream-car s2))
    (error "Division by zero")
    (mul-series s1
                (invert-series s2))))
;正接のべき級数
(display-stream
  (stream-take (div-series sine-series cosine-series) 7))
;=>
;0
;1
;0
;1/3
;0
;2/15
;0
;17/315
