(load "./load-eceval-compiler")
(load "./compiler")

(define (main args)
  (print-compiled-code
    (compile
      '(define (f x)
         (+ x (g (+ x 2))))
      'val
      'next))
  )
