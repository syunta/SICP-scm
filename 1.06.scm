(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(print (sqrt 2))
;=>
; new-if を使った版では無限のループに陥る。
; new-if は通常の手続きなので、new-if の本体の評価の前に引数の評価が起きる。
; new-if の引数 (sqrt-iter ..) が評価され続け new-if の本体が評価されることなく無限に sqrt-iter が呼び出される。
