(define (double f)
  (lambda (x)
    (f (f x))))

(define (inc x) (+ x 1))

(define (main args)
  (print ((double inc) 3)) ;=> 5
  (print
    (((double (double double)) inc) 5))
  ;=> 21
  ; double に double を渡すと、2回作用させる手続きをさらに2回作用させるので、4回作用させる手続きを返す手続きができる。
  ; double に (double double) を渡すと、4回作用させる手続きをさらに4回作用させるので、16回作用させる手続きを返す手続きができる。
  )

