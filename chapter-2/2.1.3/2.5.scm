; 2^a*3^b
; 2^4*3^5
; 2222 * 33333
;
; 2と3は互いに素なので、2^aのaがどれだけ増えても3^bのbに影響しない。
; a, b は互いに独立した数字を持つことができるので、非負の整数の対は数と算術演算だけを使って表現出来る

(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (count-divide x divisor)
  (define (iter z n)
    (if (= 1 (gcd z divisor))
      n
      (iter (/ z divisor) (+ n 1))))
  (iter x 0))

(define (car z)
  (count-divide z 2))

(define (cdr z)
  (count-divide z 3))

(define (main args)
  (print (car (cons 3 7)))
  (print (cdr (cons 3 7)))
  )
