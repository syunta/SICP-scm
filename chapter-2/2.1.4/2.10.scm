(load "./2.9")

; ゼロを跨る区間で割ると正しい最大値、最小値はが見積もれない。
; [9, 11] を [-1, 1] で割ると、除数は 0.1 や -0.1 かも知れない。
; この場合、結果は [-100, 100] になるが、Alyssaのプログラムでは [-11, 11] を返す。
; また、0.000001 など除数はいくらでも小さくできるし 0 かも知れない。
; 最大値(最小値)が無限に大きくなる可能性があるので、ゼロを跨る区間で割るのは望ましくない。

(define (div-interval x y)
  (if (< (lower-bound y) 0 (upper-bound y))
    (error "DIV-INTERVAL: division by interval spans zero")
    (mul-interval x
                  (make-interval (/ 1.0 (upper-bound y))
                                 (/ 1.0 (lower-bound y))))))

(define (main args)
  (print (div-interval (make-interval 1 2)
                       (make-interval 3 4)))
  (print (div-interval (make-interval 1 2)
                       (make-interval -3 -4)))
  (print (div-interval (make-interval 1 2)
                       (make-interval -3 4)))
  )
