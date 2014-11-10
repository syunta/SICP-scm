; eq?を使えばオブジェクトが同一性を調べられる
; memqは内部的にeq?を使っている

(define nil '())

(define (count-pairs x)
  (let ((counted nil))
    (define (go x)
      (cond ((not (pair? x)) 0)
            ((memq x counted) 0)
            (else (set! counted (cons x counted))
                  (+ (go (car x))
                     (go (cdr x))
                     1))))
    (go x)))

(define x '(a b c))
;Benの手続きでは7を返していたリストを作る
(set-car! x (cdr x))
(set-car! (cdr x) (cddr x))

(print (count-pairs x))
;=> 3
