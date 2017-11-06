(load "./1.24")

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (expmod base exp m)
  (remainder (fast-expt base exp) m))

(define (main args)
  (print (take-primes 1000 3))
  ;=> (1009 1013 1019)
  (print (take-primes 10000 3))
  ;=> (10007 10009 10037)
  (print (take-primes 100000 3))
  ;=> ...
  (print (take-primes 1000000 3))
  ;=> ...

  ; 結果は同じでステップ数も同じだが、計算に非常に時間がかかる。
  ; 高速素数テストには使えない。
  ; 例えば、 (expt 50000 100000) などはとてつもなく大きい数になる。
  ; schemeではどんなに大きい数でもメモリの許す限り表現できるが、bignumber型の計算は非常に遅い。
  ; bignumberにならないように、毎回 remainder を挟むと計算が早くなる。
  )
