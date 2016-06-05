(define (cont-frac n d k)
  (define (iter count)
    (if (> count k)
      1
      (/ (n count)
         (+ (d count)
            (iter (+ count 1))))))
  (iter 1))

(define (cont-frac-iter n d k)
  (define (iter result k)
    (if (zero? k)
      result
      (iter (/ (n k)
               (+ (d k) result))
            (- k 1))))
  (iter 1 k))

(define (main args)
  ; a

  (print (cont-frac (lambda (i) 1.0)
                    (lambda (i) 1.0)
                    11))
  ;=> k=11 で 1/Φ = 0.6180 の4桁の精度で近似できる。

  ; b
  (print (cont-frac-iter (lambda (i) 1.0)
                         (lambda (i) 1.0)
                         11))
  )
