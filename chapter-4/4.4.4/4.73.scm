(add-load-path "../../lib" :relative)
(load "query")

(define (flatten-stream stream)
  (if (stream-null? stream)
    the-empty-stream
    (interleave
      (stream-car stream)
      (flatten-stream (stream-cdr stream)))))

(define ones (cons-stream 1 ones))

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
  )
