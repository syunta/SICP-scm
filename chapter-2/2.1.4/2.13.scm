(load "./2.12")

; a,b: パーセント許容誤差
;
; (x ± ax) * (y ± by)
;
; xy
; ± bxy
; ± axy
; ± abxy  a,b は小さいので無視できる
;
; => xy ± (a + b)xy
;
; 二つの区間の積のパーセント許容誤差は単純に足算で近似して示せる。

(define x (make-center-percent 100 1))
(define y (make-center-percent 150 0.5))
(define p (mul-interval x y))

(define x2 (make-center-percent 1000 2))
(define y2 (make-center-percent 3000 3))
(define p2 (mul-interval x2 y2))

(define (main args)
  (print (percent p)) ;=> 1.499..
  (print (percent p2)) ;=> 4.99..
  )
