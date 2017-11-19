(load "./2.07")

; 区間x,yの差の最小値はxの下限とyの上限の差であり、最大値はxの上限とyの下限の差である。
; X - Y = [a - d, b - c]

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define (main args)
  (print (sub-interval (make-interval 2 5)
                       (make-interval 0 2)))
  )
