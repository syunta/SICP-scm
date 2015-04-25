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
