; eq?を使えばオブジェクトが同一性を調べられる
; memqは内部的にeq?を使っている

(define nil '())

(define count-pairs
  (let ((counted nil))
    (lambda (x)
      (cond ((not (pair? x)) 0)
            ((memq x counted) 0)
            (else (set! counted (cons x counted))
                  (+ (count-pairs (car x))
                     (count-pairs (cdr x))
                     1))))))

(define x '(a b c))
;Benの手続きでは7を返していたリストを作る
(set-car! x (cdr x))
(set-car! (cdr x) (cddr x))

(print (count-pairs x))
;=> 3
