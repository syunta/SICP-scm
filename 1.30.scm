(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (main args)
  (print (sum (lambda (x) (* x x x))
              1
              (lambda (x) (+ x 1))
              10))
  )
