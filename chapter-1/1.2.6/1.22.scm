(load "./1.21")

(define (runtime)
  (round->exact (* (expt 10 6)
                   (time->seconds (current-time)))))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
    (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes low high)
  (cond ((< high low) 'done)
        ((odd? low)
         (timed-prime-test low)
         (search-for-primes (+ low 2) high))
        (else (search-for-primes (+ 1 low) high))))

(define (main args)
  (search-for-primes 10 20)
  )
