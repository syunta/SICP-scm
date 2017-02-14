(load "./2.94")

(define (main args)
  (install-polar-package)
  (install-rectangular-package)
  (install-scheme-number-package)
  (install-rational-package)
  (install-complex-package)
  (install-polynomial-package)
  (let ((p1 (make-polynomial 'x '((2 1) (1 -2) (0 1))))
        (p2 (make-polynomial 'x '((2 11) (0 7))))
        (p3 (make-polynomial 'x '((1 13) (0 5)))))
    (let ((q1 (mul p1 p2))
          (q2 (mul p1 p3)))
      (print (greatest-common-divisor q1 q2))))
  ;(gcd-terms a b)
  ;  a: ((4 11) (3 -22) (2 18) (1 -14) (0 7))
  ;  b: ((3 13) (2 -21) (1 3) (0 5))
  ;next
  ;  a: ((3 13) (2 -21) (1 3) (0 5))
  ;  b: ((2 1458/169) (1 -2916/169) (0 1458/169))
  ;next
  ;  a: ((2 1458/169) (1 -2916/169) (0 1458/169))
  ;  b: ()
  
  ; 係数が 1458/169 の整数倍の結果となってしまう
  )
