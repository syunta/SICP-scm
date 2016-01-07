(add-load-path "../../lib" :relative)
(load "amb")

(for-each simple-ambeval
          '(
            (define count 0)
            ))

(define (main args)
  (print-ambeval
    '(let ((x (an-element-of '(a b c)))
           (y (an-element-of '(a b c))))
       (set! count (+ count 1))
       (require (not (eq? x y)))
       (list x y count))
    10)
  ;=>
  ; (a b 1)
  ; (a c 1)
  ; (b a 1)
  ; (b c 1)
  ; (c a 1)
  ; (c b 1)
  ; set!を使うと副作用がやり戻されて全て1になる
  )
