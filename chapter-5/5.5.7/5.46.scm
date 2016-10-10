(load "../../lib/regsim")
(add-load-path "../../lib" :relative)
(load "../../lib/load-eceval-compiler")
(load "../../lib/compiler")

(define fib
  (make-machine
    '(n val continue)
    (list (list '+ +) (list '< <) (list '- -))
    '(controller
       (assign continue (label fib-done))
       fib-loop
       (test (op <) (reg n) (const 2))
       (branch (label immediate-answer))
       ;; Fib(n-1)を計算するよう設定
       (save continue)
       (assign continue (label afterfib-n-1))
       (save n)                           ; nの昔の値を退避
       (assign n (op -) (reg n) (const 1)); nを n-1 に変える
       (goto (label fib-loop))            ; 再帰呼出しを実行
       afterfib-n-1                         ; 戻った時 Fib(n-1)はvalにある
       (restore n)
       (restore continue)
       ;; Fib(n-2)を計算するよう設定
       (assign n (op -) (reg n) (const 2))
       (save continue)
       (assign continue (label afterfib-n-2))
       (save val)                         ; Fib(n-1)を退避
       (goto (label fib-loop))
       afterfib-n-2                         ; 戻った時Fib(n-2)の値はvalにある
       (assign n (reg val))               ; nにはFib(n-2)がある
       (restore val)                      ; valにはFib(n-1)がある
       (restore continue)
       (assign val                        ; Fib(n-1)+Fib(n-2)
               (op +) (reg val) (reg n))
       (goto (reg continue))              ; 呼出し側に戻る. 答えはvalにある
       immediate-answer
       (assign val (reg n))               ; 基底の場合: Fib(n)=n
       (goto (reg continue))
       fib-done
       (perform (op print-stack-statistics))
       (perform (op initialize-stack)))))

(define (main args)
  ; 特殊目的fibのプッシュ数,最大深さ計測
  (for-each (lambda (n)
              (set-register-contents! fib 'n n)
              (start fib))
            (iota 7 1))

  ;(print-compiled-code
  ;  (compile
  ;    '(define (fib n)
  ;       (if (< n 2)
  ;         n
  ;         (+ (fib (- n 1)) (fib (- n 2)))))
  ;    'val
  ;    'next))

  (compile-and-go
    '(define (fib n)
       (if (< n 2)
         n
         (+ (fib (- n 1)) (fib (- n 2))))))
  ;
  ;         | n | 最大深さ | プッシュ回数
  ; --------+---+----------+-------------
  ; 翻訳    |   |     5    |    17
  ; 解釈    | 2 |    13    |    72
  ; 特殊目的|   |     2    |     4
  ; --------+---+----------+-------------
  ; 翻訳    |   |     8    |    27
  ; 解釈    | 3 |    18    |   128
  ; 特殊目的|   |     4    |     8
  ; --------+---+----------+-------------
  ; 翻訳    |   |    11    |    47
  ; 解釈    | 4 |    23    |   240
  ; 特殊目的|   |     6    |    16
  ; --------+---+----------+-------------
  ; 翻訳    |   |    14    |    77
  ; 解釈    | 5 |    28    |   408
  ; 特殊目的|   |     8    |    28
  ; --------+---+----------+-------------
  ; 翻訳    |   |    17    |   127
  ; 解釈    | 6 |    33    |   688
  ; 特殊目的|   |    10    |    48
  ; --------+---+----------+-------------
  ; 翻訳    |   |    20    |   207
  ; 解釈    | 7 |    38    |  1136
  ; 特殊目的|   |    12    |    80
  ; --------+---+----------+-------------


  ;         | オーバーヘッド定数k |   最大深さ  |       プッシュ数
  ; --------+---------------------+-------------+-------------------------
  ; 翻訳    |           3         |  3(n-1) + 2 | 10 * Fib(n+1) -  3
  ; 解釈    |          40         |   5n + 8    | 56 * Fib(n+1) - 40
  ; 特殊目的|           4         |   2(n - 1)  |  4 * Fib(n+1) -  4
  ; --------+---------------------+-------------+-------------------------
  ; 比      |
  ; --------+---------------------+-------------+-------------------------
  ; 翻訳    |           3         |      3      |           5
  ; 解釈    |          40         |      5      |          28
  ; 特殊目的|           4         |      2      |           2
  ; --------+---------------------+-------------+-------------------------

  ; 最大深さの比はfactorialと同じ。
  ; プッシュ数の比もfactorialに近い。
  )
