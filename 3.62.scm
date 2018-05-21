(load "./3.61")

(define (invert-series stream)
  (define inverts
    (scale-stream (cons-stream 1
                               (negate-stream (mul-series (stream-cdr stream)
                                                          inverts)))
                  (/ 1 (stream-car stream))))
  inverts)

(define (div-series s1 s2)
  (if (zero? (stream-car s2))
    (error "Division by zero")
    (mul-series s1
                (invert-series s2))))

;正接のべき級数
(define tan-series (div-series sine-series cosine-series))

(display-stream
  (stream-take tan-series 7))
;=>
;0
;1
;0
;1/3
;0
;2/15
;0
;17/315
