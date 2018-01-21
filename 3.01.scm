; (define (f x) ..) -> (define f (lambda (x) ..))

;(let ((x 1) (y 2))
;  (+ x y))
;
;  ↓
;
;((lambda (x y)
;   (+ x y))
; 1 2)

(define (make-accumulator sum)
  (lambda (x)
    (set! sum (+ sum x))
    sum))

(define acc1 (make-accumulator 100))
(define acc2 (make-accumulator 100))

(print (acc1 10))
(print (acc1 10))
(print (acc2 10))
(print (acc2 100))
(print (acc1 10))
