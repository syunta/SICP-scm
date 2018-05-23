(load "./normal-order-eval-apply")

(define (main args)
  (print
    (actual-value
      '(begin
         (define (id x) x)
         ((id +) 2 4)
         ; (id +) は (thunk x) を返すのでactual-valueが必要
         )
      the-global-environment))
  ;=> 6
  )
