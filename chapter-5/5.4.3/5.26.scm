(add-load-path "../../lib" :relative)
(load "../../lib/load-eceval")

(define (main args)
  (start eceval)
  ;=>
  ;(factorial 1)
  ;(total-pushes = 64 maximum-depth = 10)

  ;(factorial 2)
  ;(total-pushes = 99 maximum-depth = 10)

  ;(factorial 3)
  ;(total-pushes = 134 maximum-depth = 10)

  ;(factorial 4)
  ;(total-pushes = 169 maximum-depth = 10)

  ;(factorial 5)
  ;(total-pushes = 204 maximum-depth = 10)

  ;(factorial 6)
  ;(total-pushes = 239 maximum-depth = 10)

  ; a
  ; 最大深さは10だった

  ; b
  ; 35n + 29
  )
