(load "../../lib/eval-apply")

'((lambda (x)
    (define u 1)
    (define v 2)
    (+ x u v))
  10)

; 上記の式を評価する時の環境構造は以下の通り

; 逐次的定義
'(env
   (frame1
     (v u x) 2 1 10)
   (the-global-env
     ; 大域環境が続く
     ))

; 定義を掃き出し
; let は手続き呼び出しに変換されるので frame が1段深くなる
'(env
   (frame2
     (u v) 1 2)
   (frame1
     (x) 10)
   (the-global-env
     ; 大域環境が続く
     ))

; 余計なフレームを構成しないようにするには
; 次の式が

'((lambda (x)
    (define u 1)
    (define v 2)
    (+ x u v))
  10)

; 以下のように変換されれば良い

'((lambda (x)
    (define u '*unassigned*)
    (define v '*unassigned*)
    (set! u 1)
    (set! v 2)
    (+ x u v))
  10)
