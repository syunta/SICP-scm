(load "../../lib/stream")

(define (merge-weighted weight s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
          (let ((s1car (stream-car s1))
                (s2car (stream-car s2)))
            (cond ((< (weight s1car) (weight s2car))
                   (cons-stream s1car (merge-weighted weight (stream-cdr s1) s2)))
                  ((> (weight s1car) (weight s2car))
                   (cons-stream s2car (merge-weighted weight s1 (stream-cdr s2))))
                  (else
                    (cons-stream s1car
                                 (merge-weighted weight (stream-cdr s1) s2))))))))

(define (weighted-pairs weight s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (merge-weighted
      weight
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (weighted-pairs weight (stream-cdr s) (stream-cdr t)))))

(define (sum-pair pair) (accumlate + 0 pair))

(define (main args)
  (display-stream
    (stream-take (weighted-pairs sum-list integers integers) 10))
  ;=>
  ;(1 1)
  ;(1 2)
  ;(1 3)
  ;(2 2)
  ;(1 4)
  ;(2 3)
  ;(1 5)
  ;(2 4)
  ;(3 3)
  ;(1 6)
  ;(2 5)
  )
