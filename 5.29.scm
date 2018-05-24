(load "./load-eceval")

(define (main args)
  (start eceval)
  ;=>
  ;(fib 2)
  ;(total-pushes = 72 maximum-depth = 13)

  ;(fib 3)
  ;(total-pushes = 128 maximum-depth = 18)

  ;(fib 4)
  ;(total-pushes = 240 maximum-depth = 23)

  ;(fib 5)
  ;(total-pushes = 408 maximum-depth = 28)

  ;(fib 6)
  ;(total-pushes = 688 maximum-depth = 33)

  ;(fib 7)
  ;(total-pushes = 1136 maximum-depth = 33)


  ; a
  ;
  ; n >= 2 での最大深さは 5n + 8 の増加

  ; b
  ;
  ; Fib(n) は Fib(n-1) と Fib(n-2) のを使って、Fib(n) = Fib(n-1) + Fib(n-2) で表せるので、
  ; プッシュ数 S(n) も S(n) = S(n-1) + S(n-2) で表せるはずである。
  ; 実際にはオーバーヘッド定数k を足して S(n) = S(n-1) + S(n-2) + k で表せる。
  ; このとき k = 40
  ;
  ; (fib n): a * Fib(n+1) + b = S(n)
  ; (fib 2): a * 2 + b  = 72
  ; (fib 3): a * 3 + b  = 128
  ; 上記の連立方程式を解くと,
  ; a = 56, b = -40
  ; よって, S(n) = 56 * Fib(n+1) - 40
  )
