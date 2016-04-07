(define (improve guess x)
  (let* ((quotient (/ x guess))
         (average (average guess (/ x guess))))
    (print #"~guess  ~|x|/~|guess|=~quotient  (~|guess|+~|quotient|)/2=~average")
    average))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x)
               x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (main args)
  (print (sqrt 0.0001))
  ;=>
  ; 0.0001 の平方根は 0.01 である。
  ; しかし、計算が続くと予測値が 0.0323... となり、
  ; (0.0323 * 0.0323) - 0.0001 ≒ 0.0009 で good-enough? を満たしてしまい、正しい結果を生じない。

  (print (sqrt 10000000000000))
  ;=>
  ; 無限のループに陥る。
  ; 計算が続くと平均値を求める際、以下のような計算が起きる。
  ; 3162277.6601683795 + 3162277.660168379 / 10000000000000
  ; 3162277.6601683795 + 3162277.660168379 の本来の結果は 6324555.3203367585 であるが、
  ; 精度が足りず 6324555.3203367589 に丸められる。
  ; 6324555.3203367589 / 2 は 3162277.6601683795 となるので、以後、同じ予測値が生成され続け無限ループになる。
  )
