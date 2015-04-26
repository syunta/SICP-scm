(load "./3.71")

; cubic-sumsの連続する3つの要素が等しいことを調べればよい

(define (Ramanujan-iter s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (if (and (= s0 s1) (= s0 s2))
      (cons-stream s0
                   (Ramanujan-iter (stream-cdddr s)))
      (Ramanujan-iter (stream-cdr s)))))

(define Ramanujan-numbers
  (Ramanujan-iter cubic-sums))

(define (main args)
  (display-stream
    (stream-take Ramanujan-numbers 0))
  ;=> 87539319
  )
