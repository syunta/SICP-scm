(add-load-path "../../lib" :relative)
(load "../../lib/load-eceval-compiler")
(load "../../lib/compiler")

(define (main args)
  (print-compiled-code
    (compile
      '(define (f x)
         (+ x (g (+ x 2))))
      'val
      'next))
  )
