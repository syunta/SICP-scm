(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (inc x) (+ x 1))

(define (main args)
  (print ((compose square inc) 6)) ;=> 49
  (print ((compose not zero?) 0)) ;=> #f
  )
