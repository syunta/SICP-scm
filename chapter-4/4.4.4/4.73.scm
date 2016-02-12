(add-load-path "../../lib" :relative)
(load "query")

(define (flatten-stream stream)
  (if (stream-null? stream)
    the-empty-stream
    (interleave
      (stream-car stream)
      (flatten-stream (stream-cdr stream)))))

(define ones (cons-stream 1 ones))

(define assersions-one
  (cons-stream '(one 1) assersions-one))

(set! THE-ASSERTIONS assersions-one)

(define (main args)
  ; 入力が無限ストリームである場合、再帰的に呼び出されるinterleaveの2番目の引数
  ; (flatten-stream (stream-cdr stream))
  ; が評価され続け、無限ループに陥る。
  (display-stream 
    (stream-take
      (stream-flatmap
        (lambda (x)
          (cons-stream (* x 2) the-empty-stream))
        ones)
      10))
  ;=>
  ; 無限ループに陥る。陽にdelayを使う場合は問題ない。
  ; 質問システムにおいては、無限の表明や無限の規則を持つ場合、無限ループに陥る。
  (print-qeval '(?x 1))
  ;=>
  ; 無限ループに陥り、出力が返らない。
  ; 陽にdelayを使う場合、(one 1) が無限に出力される。
  ; この違いは、例えば質問システムが任意の数の結果を印字し
  ; `(try-again)`
  ; で残りの結果を印字する、という仕様だった場合、
  ; delayを陽に使うことでシステムが無限に印字を続けることを避けられるが、
  ; delayを陽に使わないと無限にループ落ち込んでしまい望ましくない.
  )
