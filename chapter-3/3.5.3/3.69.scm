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

(define (triples s t u)
  (cons-stream
    (list (stream-car s)
          (stream-car t)
          (stream-car u))
    (interleave
      (stream-map (lambda (x) (cons (stream-car s) x))
                  (pairs (stream-cdr t) (stream-cdr u)))
      (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

(define (pythagoras? triple)
  (= (+ (expt (first triple) 2)
        (expt (second triple) 2))
     (expt (third triple) 2)))

(define (pythagorases s t u)
  (stream-filter pythagoras?
                 (triples s t u)))

(define (main args)
  (display-stream
    (stream-take (pythagorases integers integers integers) 4)))
;=>
;(3 4 5)
;(6 8 10)
;(5 12 13)
;(9 12 15)
;(8 15 17)
