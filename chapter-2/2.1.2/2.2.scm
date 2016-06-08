(define (make-segment p1 p2) (cons p1 p2))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (average . args)
  (/ (apply + args) (length args)))

(define (midpoint-segment s)
  (let ((midx (average (x-point (end-segment s))
                       (x-point (start-segment s))))
        (midy (average (y-point (end-segment s))
                       (y-point (start-segment s)))))
    (make-point midx midy)))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define test1 (make-segment (make-point 0 0) (make-point 2 4)))
(define test2 (make-segment (make-point 3 6) (make-point 1 2)))

(define (main args)
  (print-point (midpoint-segment test1))
  (print-point (midpoint-segment test2))
  )
