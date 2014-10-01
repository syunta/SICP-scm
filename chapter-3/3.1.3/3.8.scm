; 引数を評価する順番を制御する術がないので、同じ実装のf1,f2を用意し、引数を入れ替えて実験

(define f1
  (let ((a 1))
    (lambda (x)
      (set! a (* a x))
      a)))

(define f2
  (let ((a 1))
    (lambda (x)
      (set! a (* a x))
      a)))

(print (+ (f1 0) (f1 1))) ; => 0
(print (+ (f2 1) (f2 0))) ; => 1
