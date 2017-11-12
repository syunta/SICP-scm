(load "./1.40")
(load "./1.44")

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (nthrt-test x n c)
  (fixed-point
    ((repeated average-damp c) (lambda (y)
                                 (/ x (expt y (- n 1)))))
    1.0))

(define (nthrt x n)
  (fixed-point
    ((repeated average-damp (truncate (log n 2)))
     (lambda (y) (/ x (expt y (- n 1)))))
    1.0))

(define (main args)
  (print (nthrt-test 2 3 1))
  (print (nthrt-test 2 4 2)) ;平均緩和1回だと収束しない
  (print (nthrt-test 2 5 2))
  (print (nthrt-test 2 6 2))
  (print (nthrt-test 2 7 2))
  (print (nthrt-test 2 8 3)) ;平均緩和2回だと収束しない
  (print (nthrt-test 2 9 3))
  (print (nthrt-test 2 10 3))
  (print (nthrt-test 2 11 3))
  (print (nthrt-test 2 12 3))
  (print (nthrt-test 2 13 3))
  (print (nthrt-test 2 14 3))
  (print (nthrt-test 2 15 3))
  (print (nthrt-test 2 16 4)) ;平均緩和3回だと収束しない
  ;=>
  ; 平均緩和の必要回数は、n乗根のnが2倍になる毎に +1 されるから、
  ; log_2(n) の増加
  (newline)
  (print (nthrt 2 3))
  (print (nthrt 2 4))
  (print (nthrt 2 5))
  (print (nthrt 2 6))
  (print (nthrt 2 7))
  (print (nthrt 2 8))
  (print (nthrt 2 9))
  (print (nthrt 2 10))
  (print (nthrt 2 11))
  (print (nthrt 2 12))
  (print (nthrt 2 13))
  (print (nthrt 2 14))
  (print (nthrt 2 15))
  (print (nthrt 2 16))
  )
