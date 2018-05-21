(load "./3.70")

(define (square-sum twins)
  (+ (square (car twins)) (square (cadr twins))))

(define (stream-group-by weight s)
  (define (go s key element)
    (let ((result (cons key element)))
      (if (stream-null? s)
        (cons-stream result
                     the-empty-stream)
        (let ((next-key (weight (stream-car s))))
          (if (= key next-key)
            (go (stream-cdr s) key (cons (stream-car s) element))
            (cons-stream result
                         (go s next-key '())))))))
  (go s (weight (stream-car s)) '()))

(define grouping-square-sums
  (stream-group-by square-sum
                   (weighted-pairs square-sum
                                   integers
                                   integers)))

(define (main args)
  (display-stream
    (stream-take (stream-filter (lambda (x) (= 3 (length (cdr x))))
                                grouping-square-sums)
                 5))
  ;=>
  ;(325 (10 15) (6 17) (1 18))
  ;(425 (13 16) (8 19) (5 20))
  ;(650 (17 19) (11 23) (5 25))
  ;(725 (14 23) (10 25) (7 26))
  ;(845 (19 22) (13 26) (2 29))
  ;(850 (15 25) (11 27) (3 29))
  )
