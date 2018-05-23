(load "./normal-order-eval-apply")

(append! primitive-procedures (list (list 'print print)))
(define the-global-environment (setup-environment))

(define (main args)
  ; このテストでは基本手続きprintを使っているが、count でも actual-valueで印字されるので結果は同じ
  (actual-value
    '(begin

       (define count 0)

       (define (id x)
         (set! count (+ count 1))
         x)

       (print (define w (id (id 10))))
       ; 外側のidの引数がthunkされる
       ; (id (thunk (id 10)))
       ;   id 本体の評価
       ;     set!,+ は基本手続きのため引数は評価されてcount=1となる
       ;     (set! count (+ count 1))
       ;     id の返り値はthunk
       ;     (thunk (id 10))
       ; w には (id 10) のthunkが束縛される
       ; (define w (thunk (id 10))
       ; => ok

       (print count)
       ; この時点では set! の呼び出しは1回
       ; => 1
       (print w)
       ; w には (id 10) のthunkが束縛されている
       ; これが駆動ループで印字されるとき、actual-valueで値が取り出される
       ; (thunk (id 10)
       ;   id 本体の評価
       ;     set!,+ は基本手続きのため引数は評価されてcount=2となる
       ;     (set! count (+ count 1))
       ;     id の返り値は10
       ;     10
       ; => 10
       (print count)
       ; この時点では set! の呼び出しは2回
       ; => 2
       (print count)
       ; 以降、w の thunk はメモ化されるので、何回呼び出しても結果は2
       ; => 2
       )
    the-global-environment)
  )
