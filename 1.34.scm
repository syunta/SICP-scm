(define (f g)
  (g 2))

(define (main args)
  (f f)
  ; => 
  ; (f f)
  ; (f 2)
  ; (2 2)
  ;
  ; 数値が手続きとして評価されエラーとなる。
  )
