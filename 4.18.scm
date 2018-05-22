(load "./stream")
(define integral delayed-integral)

'(lambda (x)
   (let ((u '*unassigned*)
         (v '*unassigned*))
     (let ((a <e1>)
           (b <e2>))
       (set! u a)
       (set! v b))
     <e3>))

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

; 問題文の掃き出し戦略
; (b (stream-map f y))
; の評価で y はまだ *unassigned* なのでエラーになる
(define (solve f y0 dt)
  (let ((y '*unassigned*)
        (dy '*unassigned*))
    (let ((a (integral (delay dy) y0 dt))
          (b (stream-map f y)))
      (set! y a)
      (set! dy b)
      y)))

; 本文の掃き出し戦略
; dy は delay されその force はcons-streamでさらに遅延される
; y のstreamの残りが評価される頃には dy もset!されているので問題なく動く
(define (solve f y0 dt)
  (let ((y '*unassigned*)
        (dy '*unassigned*))
    (set! y (integral (delay dy) y0 dt))
    (set! dy (stream-map f y))
    y))

(define (main args)
  (print (stream-ref (solve (lambda (x) x) 1 0.001) 100))
  )
