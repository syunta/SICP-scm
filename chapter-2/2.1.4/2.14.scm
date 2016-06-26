(load "./2.13")

(define A (make-center-width 100 1))
(define B (make-center-width 200 1))
(define A/A (div-interval A A))
(define A/B (div-interval A B))

(define (main args)
  (print (center A/A))
  (print (percent A/A))
  (print (center A/B))
  (print (percent A/B))
  ; div-intaerval の度に誤差が大きくなる
  ; par1, par2 では div-interval の評価回数が違うので、計算結果が異なったものとなると考えられる。
  )
