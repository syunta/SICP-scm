;(load "../../lib/eval-apply")
(load "../../lib/normal-order-eval-apply")

(define test
  '(begin
     (define (unless condition usual-value exceptional-value)
       (if condition exceptional-value usual-value))
     (define (factorial n)
       (unless (= n 1)
         (* n (factorial (- n 1)))
         1))
     (factorial 5)))

(define (main args)
  ;(print (eval test the-global-environment))
  ; 通常の作用的順序のSchemeでは (* n (factorial (- n 1))) が評価され続け、無限のループに陥る
  (print (actual-value test the-global-environment))
  ; 正規順序の評価器では動く
  )
