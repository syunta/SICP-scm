(load "./normal-order-eval-apply")

(append! primitive-procedures (list (list 'print print)))
(define the-global-environment (setup-environment))

(define (force-it-without-memo obj)
  (if (thunk? obj)
    (actual-value (thunk-exp obj) (thunk-env obj))
    obj))

(define (main args)
  ; thunk された部分式の引数への参照が多い時、遅くなる

  ; メモ化した時
  (actual-value
    '(begin

       (define count 0)

       (define (id x)
         (set! count (+ count 1))
         x)

       (define (square x)
         (* x x))

       (print (square (id 10)))
       ; square の引数がthunkされる
       ; (square (thunk (id 10))
       ;   square 本体の評価
       ;     * は基本手続きなので引数のthunkがforceされる
       ;     (* (thunk (id 10)) (thunk (id 10)))
       ;       第1引数 (thunk (id 10)) の評価
       ;         set!,+ は基本手続きのため引数は評価されてcount=1となる
       ;         (set! count (+ count 1))
       ;         10
       ;       第2引数 (thunk (id 10)) の評価
       ;         メモ化されているので即10を返す
       ;         10
       ;     (* 10 10)
       ;=> 100

       (print count)
       ; set! の呼び出しは1回のみ
       ;=> 1
       )
    the-global-environment)

  ; メモ化しない時
  (set! force-it force-it-without-memo)

  (actual-value
    '(begin

       (define count 0)

       (define (id x)
         (set! count (+ count 1))
         x)

       (define (square x)
         (* x x))

       (print (square (id 10)))
       ; square の引数がthunkされる
       ; (square (thunk (id 10))
       ;   square 本体の評価
       ;     * は基本手続きなので引数のthunkがforceされる
       ;     (* (thunk (id 10)) (thunk (id 10)))
       ;       第1引数 (thunk (id 10)) の評価
       ;         set!,+ は基本手続きのため引数は評価されてcount=1となる
       ;         (set! count (+ count 1))
       ;         10
       ;       第2引数 (thunk (id 10)) の評価
       ;         set!,+ は基本手続きのため引数は評価されてcount=2となる
       ;         (set! count (+ count 1))
       ;         10
       ;     (* 10 10)
       ;=> 100

       (print count)
       ; set! の呼び出しは2回
       ;=> 2
       )
    the-global-environment)
  )
