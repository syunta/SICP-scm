(define (double f)
  (lambda (x)
    (f (f x))))

(define (inc x) (+ x 1))

(define (main args)
  (print ((double inc) 3)) ;=> 5
  (print
    (((double (double double)) inc) 5))
  ;=> 21
  ;
  ; (double double) は以下のような関数を返す
  ;
  ; (lambda (x) ((lambda (x) (f (f x)))
  ;               ((lambda (x) (f (f x))) x)))
  ;
  ; (double (double double)) 上記の関数のfにそれぞれ同じ関数を入れたものを返す
  ;
  ; よって、評価は16回起きる。
  )
