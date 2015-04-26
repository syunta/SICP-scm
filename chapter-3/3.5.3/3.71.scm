(load "./3.70")

(define (cubic-sum pair)
  (+ (cube (car pair)) (cube (cadr pair))))

(define cubic-sums
  (stream-map cubic-sum
              (weighted-pairs cubic-sum
                              integers
                              integers)))

(define (Ramanujan-iter s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1)))
    (if (= s0 s1)
      (cons-stream s0
                   (Ramanujan-iter (stream-cddr s)))
      (Ramanujan-iter (stream-cdr s)))))

(define Ramanujan-numbers
  (Ramanujan-iter cubic-sums))

(define (main args)
  (display-stream
    (stream-take Ramanujan-numbers 5))
  ;=>
  ;1729
  ;4104
  ;13832
  ;20683
  ;32832
  ;39312
  )
