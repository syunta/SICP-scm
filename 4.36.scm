(load "./4.35")


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
; 探索の上限値を設ければよい。

; k = 1 2 3 ... n
; i < k
; i <= j < k
(for-each simple-ambeval
          '((define (an-integer-starting-from n)
              (amb n (an-integer-starting-from (+ n 1))))
            (define (a-pythagorean-triples)
              (let ((k (an-integer-starting-from 1)))
                (let ((i (an-integer-between 1 (- k 1))))
                  (let ((j (an-integer-between i (- k 1))))
                    (require (= (+ (* i i) (* j j)) (* k k)))
                    (list i j k)))))))

(define (main args)
  (print-ambeval '(a-pythagorean-triples) 10)
  ;=>
  ; (3 4 5)
  ; (6 8 10)
  ; (5 12 13)
  ; (9 12 15)
  ; (8 15 17)
  ; (12 16 20)
  ; (7 24 25)
  ; (15 20 25)
  ; (10 24 26)
  ; (20 21 29)
  ; To be continued ...
  )
