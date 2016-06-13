(load "./2.8")

(define (width x)
  (/ (- (upper-bound x) (lower-bound x)) 2.0))

(define (main args)
  ; x = [a, b]  y = [c, d]
  ;
  ; width(x + y) => width(x) + width(y) であることを示す
  ;
  ; x + y = [a + c, b + d]
  ; width(x + y) = { (b + d) - (a + c) } / 2 = (b + d - a - c) / 2
  ;
  ; width(x) + width(y) = (b - a) / 2 + (d - c) / 2 = (b - a + d - c) / 2
  ;
  (print (width (add-interval (make-interval 2 4)
                              (make-interval 3 6))))
  ;=> 2.5
  (print (+ (width (make-interval 2 4))
            (width (make-interval 3 6))))
  ;=> 2.5
  (print (width (add-interval (make-interval -3 4)
                              (make-interval 0 6))))
  ;=> 6.5
  (print (+ (width (make-interval -3 4))
            (width (make-interval 0 6))))
  ;=> 6.5

  ; width(x - y) => width(x) + width(y) であることを示す
  ;
  ; x - y = [a - d, b - c]
  ; width(x - y) = { (b - c) - (a - d) } / 2 = (b - c - a + d) / 2
  ;
  ; width(x) + width(y) = (b - a) / 2 + (d - c) / 2 = (b - a + d - c) / 2
  ;
  (print (width (sub-interval (make-interval 4 5)
                              (make-interval 1 2))))
  ;=> 1.0
  (print (+ (width (make-interval 4 5))
            (width (make-interval 1 2))))
  ;=> 1.0
  (print (width (sub-interval (make-interval 0 5)
                              (make-interval 2 3))))
  ;=> 3.0
  (print (+ (width (make-interval 0 5))
            (width (make-interval 2 3))))
  ;=> 3.0

  ; width(x * y) => width(x) + width(y) は成り立たないことを示す
  (print (width (mul-interval (make-interval 0 0)
                              (make-interval 0 2))))
  ;=> 0.0
  (print (+ (width (make-interval 0 0))
            (width (make-interval 0 2))))
  ;=> 1.0
  (print (width (mul-interval (make-interval 2 3)
                              (make-interval 1 4))))
  ;=> 5.0
  (print (+ (width (make-interval 2 3))
            (width (make-interval 1 4))))
  ;=> 2.0

  ; width(x / y) => width(x) + width(y) は成り立たないことを示す
  (print (width (div-interval (make-interval 1 8)
                              (make-interval 4 9))))
  ;=> 0.944
  (print (+ (width (make-interval 1 8))
            (width (make-interval 4 9))))
  ;=> 6.0
  (print (width (mul-interval (make-interval -2 2)
                              (make-interval -1 1))))
  ;=> 2.0
  (print (+ (width (make-interval -2 2))
            (width (make-interval -1 1))))
  ;=> 3.0
  )
