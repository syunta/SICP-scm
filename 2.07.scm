(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))


(define (div-interval x y)
  (mul-interval x 
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define (make-interval a b) (cons a b))
(define (upper-bound p) (cdr p))
(define (lower-bound p) (car p))

(define (main args)
  (print (add-interval (make-interval 1 2)
                       (make-interval 5 9)))
  (print (mul-interval (make-interval 1 2)
                       (make-interval 5 9)))
  (print (div-interval (make-interval 10 21)
                       (make-interval 5 4)))
  )
