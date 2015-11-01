(add-load-path "../../lib" :relative)
(load "amb")

; amb の探索は単純で、最初に見つかった選択肢を選ぶ。
; よって、以下のように a-pythagorean-triples を定義してしまうと

'(define (a-pythagorean-triples)
   (let ((i (an-integer-starting-from 1)))
     (let ((j (an-integer-starting-from i)))
       (let ((k (an-integer-starting-from j)))
         (require (= (+ (* i i) (* j j)) (* k k)))
         (list i j k)))))

; (1 1 1)
; (1 1 2)
; (1 1 3)
; (1 1 4)
; ...

; のように最初に i=1,j=1 のパターンを無限に探索し続けることとなり、適切ではない。
