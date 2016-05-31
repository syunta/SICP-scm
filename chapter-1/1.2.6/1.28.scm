(load "./1.24")

(define (nontrivial? a n)
  (and (not (= a 1))
       (not (= a (- n 1)))
       (= (remainder (square a) n) 1)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (let ((root (expmod base (/ exp 2) m)))
           (if (nontrivial? root m)
             0
             (remainder (square root) m))))
        (else
          (remainder (* base (expmod base (- exp 1) m))
                     m))))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((miller-rabin-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (prime? n) (fast-prime? n 5))

(define (main args)
  (print (map prime? '(1009 1013 1019)))
  (print (map prime? '(10007 10009 10037)))
  (print (map prime? '(100003 100019 100043)))
  (print (map prime? '(1000003 1000033 1000037)))

  (print (map prime? '(561 1105 1729 2465 2821 6601)))
  )
