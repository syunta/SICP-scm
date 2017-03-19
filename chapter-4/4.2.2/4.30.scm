(load "../../lib/normal-order-eval-apply")

(define (eval-sequence-cy exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (actual-value (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(append! primitive-procedures (list (list 'list list)
                                    (list 'newline newline)
                                    (list 'display display)
                                    (list 'print print)))
(define the-global-environment (setup-environment))

(define (main args)
  ; a
  (actual-value
    '(begin

       (define (for-each proc items)
         (if (null? items)
           'done
           (begin (proc (car items))
                  (for-each proc (cdr items)))))

       (for-each (lambda (x) (newline) (display x))
                 (list 57 321 88))
       ;=>
       ; 57
       ; 321
       ; 88
       )
    the-global-environment)
  ; (begin
  ;   (proc (car items))
  ;   (for-each proc (cdr items)))
  ;
  ;     (eval (proc (car items))) は次のように評価される
  ;       ((thunk (lambda ...)) (car (thunk '(57 321 88))))
  ;       演算子の値を取り出すため演算子部分が force される
  ;       ((lambda ...) (car (thunk '(57 321 88))))
  ;       car は基本手続きのため引数が force される
  ;       ((lambda ...) (car '(57 321 88)))
  ;       car が評価される
  ;       ((lambda ...) 57)
  ;       手続き呼び出しが評価される
  ;         (newline)
  ;         (display (thunk 57))
  ;         newline,display は基本手続きのため引数 force されて57が印字される
  ;
  ; eval-sequence の各節が手続き呼び出しであれば Cy の心配は無意味

  ; b
  (actual-value
    '(begin

       (define (p1 x)
         (set! x (cons x '(2)))
         x)

       (define (p2 x)
         (define (p e)
           e
           x)
         (p (set! x (cons x '(2)))))

       (print (p1 1))
       ; p1 は合成手続きなので引数は thunk される
       ; (p1 (thunk 1))
       ; p1 本体の評価
       ;   全て基本手続きなので引数も force されて x に (1 2) が代入される
       ;   (eval (set! x:(thunk 1) (cons x:(thunk 1) '(2))))
       ;   p1 は x:(1 2) を返す
       ;   (eval x:(1 2))
       ; (1 2) が印字される
       ;=> (1 2)
       (print (p2 1))
       ; p2 は合成手続きなので引数は thunk される
       ; (p2 (thunk 1))
       ; p2 本体の評価
       ;   p は合成手続きなので引数は thunk される
       ;   (eval (p (thunk (set! x:(thunk 1) (cons x:(thunk 1) '(2))))))
       ;   p の本体の評価
       ;     e の評価は thunk を返すだけで終わる(中身の set! は評価されない)
       ;     (eval e:(thunk (set! x:(thunk 1) (cons x:(thunk 1) '(2)))))
       ;     p は x:(thunk 1) を返す
       ;     (eval x:(thunk 1))
       ; (thunk 1) を印字するため force されて値が取り出される
       ;=> 1
       )
    the-global-environment)

  ; Cy D. Fect の変更後
  (set! eval-sequence eval-sequence-cy)
  (actual-value
    '(begin

       (define (p1 x)
         (set! x (cons x '(2)))
         x)

       (define (p2 x)
         (define (p e)
           e
           x)
         (p (set! x (cons x '(2)))))

       (print (p1 1))
       ; ほぼ同様
       ;=> (1 2)
       (print (p2 1))
       ; p2 は合成手続きなので引数は thunk される
       ; (p2 (thunk 1))
       ; p2 本体の評価
       ;   p は合成手続きなので引数は thunk される
       ;   (actual-value (p (thunk (set! x:(thunk 1) (cons x:(thunk 1) '(2))))))
       ;   p の本体の評価
       ;     e を actual-value するので、中身の set! が実行され x に (1 2) が代入される
       ;     (actual-value e:(thunk (set! x:(thunk 1) (cons x:(thunk 1) '(2)))))
       ;     p は x:(1 2) を返す
       ;     (actual-value x:(1 2))
       ; (1 2) が印字される
       ;=> (1 2)
       )
    the-global-environment)

  ; c
  (actual-value
    '(begin

       (define (for-each proc items)
         (if (null? items)
           'done
           (begin (proc (car items))
                  (for-each proc (cdr items)))))

       (for-each (lambda (x) (newline) (display x))
                 (list 57 321 88))
       ;=>
       ; 57
       ; 321
       ; 88
       )
    the-global-environment)
  ; 変更前: (eval (proc (car items)))
  ; 変更後: (actual-value (proc (car items)))
  ;
  ; actual-value は eval の結果が thunk だった時に force する
  ; この場合 eval の結果は (display x) の返り値なので、 thunk ではないので影響はない
  ; (しかも値に意味はないので評価されようがされまいが関係ない)

  ; d
  ; そもそもノンストリクトな処理系に副作用を混ぜない
  ; またはここだけはストリクトに評価することを許すオプションのようなものを付ける
  )
