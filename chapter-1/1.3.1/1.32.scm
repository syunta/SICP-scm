(load "./1.31")

; a

(define (accumulate combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner (term a)
              (accumulate combiner null-value term (next a) next b))))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

; b

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a)
            (combiner result (term a)))))
  (iter a null-value))

(define (main args)
  (print (sum identity 1 inc 5))
  (print (product identity 1 inc 5))

  (print (accumulate-iter + 0 identity 1 inc 5))
  (print (accumulate-iter * 1 identity 1 inc 5))
  )
