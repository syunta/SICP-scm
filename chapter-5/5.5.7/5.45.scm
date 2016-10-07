(add-load-path "../../lib" :relative)
(load "../../lib/load-eceval-compiler")
(load "../../lib/compiler")


(define (main args)
  (print-compiled-code
    (compile
      '(define (factorial n)
         (if (= n 1)
           1
           (* (factorial (- n 1)) n)))
      'val
      'next))

  ;(compile-and-go
  ;  '(define (factorial n)
  ;     (if (= n 1)
  ;       1
  ;       (* (factorial (- n 1)) n))))
  ; a
  ;
  ;(factorial 1)
  ;(total-pushes = 7 maximum-depth = 3)
  ;
  ;(factorial 2)
  ;(total-pushes = 13 maximum-depth = 5)
  ;
  ;(factorial 3)
  ;(total-pushes = 19 maximum-depth = 8)
  ;
  ;(factorial 4)
  ;(total-pushes = 25 maximum-depth = 11)
  ;
  ;(factorial 5)
  ;(total-pushes = 31 maximum-depth = 14)
  ;
  ;         | 最大深さ       | プッシュ回数
  ; --------+----------------+----------------
  ; 翻訳    |  3(n - 1) + 2  |  6n + 1
  ; 解釈    |  5n + 3        |  32(n - 1) + 16
  ; 特殊目的|  2n - 2        |  2n - 2
  ; --------+----------------+----------------
  ; 比率    |
  ; --------+----------------+----------------
  ; 翻訳    |       3        |       3
  ; 解釈    |       5        |      16
  ; 特殊目的|       2        |       1
  ; --------+----------------+----------------
  
  ; b
  ;
  ; 問題5.38のオープンコード生成の支援。
  ; これだけで最大深さプッシュ回数が 2n - 2 になる。
  )
