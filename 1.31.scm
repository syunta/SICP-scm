(define (identity x) x)
(define (inc x) (+ x 1))

(define (product term a next b)
  (if (> a b)
    1
    (* (term a)
       (product term (next a) next b))))

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (* result (term a)))))
  (iter a 1))

(define (factorial n)
  (product identity 1 inc n))

(define (make-pi)
  (define (f x)
    (if (even? x)
      (+ x 2)
      (+ x 1)))
  (define (f2 x)
    (if (even? x)
      (+ x 1)
      (+ x 2)))
  (* 4.0
     (/ (product f 1 inc 1000)
        (product f2 1 inc 1000))))

(define (main args)
  (print (factorial 5))
  (print (make-pi))
  )
