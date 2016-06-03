(add-load-path "../1.2.6/" :relative)
(load "1.21")
(load "./1.31")

(define (prime? n)
  (and (not (= n 1))
       (= n (smallest-divisor n))))

(define (filtered-accumulate combiner null-value term pred a next b)
  (cond ((> a b) null-value)
        ((pred a)
         (combiner (term a)
                   (filtered-accumulate combiner null-value term
                                        pred (next a) next b)))
        (else
          (filtered-accumulate combiner null-value term
                               pred (next a) next b))))

; a

(define (f1 a b)
  (filtered-accumulate + 0 square prime? a inc b))

; b

(define (f2 n)
  (filtered-accumulate * 1 identity
                       (lambda (i) (= (gcd i n) 1))
                       1 inc (- n 1)))

(define (main args)
  (print (f1 1 20))
  (print (f2 10))
  )
