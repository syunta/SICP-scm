(define (f n)
  (if (< n 3)
    n
    (+ (f (- n 1))
       (* 2 (f (- n 2)))
       (* 3 (f (- n 3))))))

; f(2)=2, f(1)=1, f(0)=0 で初期化し、

; a <- a + 2b + 3c
; b <- a
; c <- b

; の同時の変換を繰り返す。
; n回の変更の後、a, b, c はそれぞれ f(n+2), f(n+1), f(n) に等しい。

(define (f-iter n)
  (define (go a b c count)
    (cond ((= count 0) c)
          ((= count 1) b)
          ((= count 2) a)
          (else (go (+ a (* 2 b) (* 3 c))
                    a
                    b
                    (- count 1)))))
  (go 2 1 0 n))

(define (main args)
  (print (f 1))
  (print (f 2))
  (print (f 3))
  (print (f 4))
  (print (f 5))
  (print (f-iter 1))
  (print (f-iter 2))
  (print (f-iter 3))
  (print (f-iter 4))
  (print (f-iter 5))
  )
