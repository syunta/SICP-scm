(load "./2.44")

(define (split f1 f2)
  (define (split painter n)
    (if (= n 0)
      painter
      (let ((smaller (split painter (- n 1))))
        (f1 painter (f2 smaller smaller)))))
  split)

(define right-split (split beside below))
(define up-split (split below beside))
