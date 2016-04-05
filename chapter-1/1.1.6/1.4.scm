(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

(define (main args)
  (print (a-plus-abs-b 5 3))
  ;=> 5 + 3 = 8
  (print (a-plus-abs-b 5 -3))
  ;=> 5 - (-3) = 8
  )
