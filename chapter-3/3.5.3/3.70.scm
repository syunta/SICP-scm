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

(define (sum-pair pair) (accumulate + 0 pair))

(define (b pair)
  (+ (* 2 (car pair))
     (* 3 (cadr pair))
     (* 5 (car pair) (cadr pair))))

(define (divisible? x divisor)
  (zero? (remainder x divisor)))

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
  (newline)
  (display-stream
    (stream-take (stream-filter (lambda (pair)
                                  (every (lambda (dividend)
                                           (every (lambda (divisor)
                                                    (not (divisible? dividend divisor)))
                                                  '(2 3 5)))
                                         pair))
                                (weighted-pairs b integers integers)) 10))
  ;=>
  ;(1 1)
  ;(1 7)
  ;(1 11)
  ;(1 13)
  ;(1 17)
  ;(1 19)
  ;(1 23)
  ;(1 29)
  ;(1 31)
  ;(7 7)
  ;(1 37)
  )
