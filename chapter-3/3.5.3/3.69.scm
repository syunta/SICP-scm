(load "../../lib/stream")

(define (pairs s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (pairs (stream-cdr s) (stream-cdr t)))))

(define first  car)
(define second cadr)
(define third  caddr)

(define (i<=j<=k? seq)
  (and (<= (first seq) (second seq) (third seq))))

(define (triples s t u)
  (stream-filter i<=j<=k?
                 (stream-map (lambda (x) (cons (first x) (second x)))
                             (pairs s (pairs t u)))))

(define (pythagoras? triple)
  (= (+ (expt (first triple) 2)
        (expt (second triple) 2))
     (expt (third triple) 2)))

(define (pythagorases s t u)
  (stream-filter pythagoras?
                 (triples s t u)))

(define (main args)
  (display-stream
    (stream-take (pythagorases integers integers integers) 2)))
;=>
;(3 4 5)
;(6 8 10)
;(5 12 13)
