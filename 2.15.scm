(load "./2.14")

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(define x (make-center-width 10 1))
(define y (make-center-width 20 1))


(define (main args)
  (print (par1 x y))
  (print (center (par1 x y)))
  (print (percent (par1 x y)))
  (print (par2 x y))
  (print (center (par2 x y)))
  (print (percent (par2 x y)))
  ; 中央値は大体同じだが、par1の方がパーセント許容誤差が大きい。
  ; 不確かな数を表現する変数(誤差を持つ区間)同士を計算する度に誤差が大きくなる。
  ; このため par2 の方が良い。
  )
