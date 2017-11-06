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

(define (take-primes low count)
  (define (iter primes n c)
    (cond ((= 0 c) (reverse primes))
          ((odd? n)
           (if (prime? n)
             (iter (cons n primes) (+ n 2) (- c 1))
             (iter primes (+ n 2) c)))
          (else (iter primes (+ n 1) c))))
  (iter '() low count))

(define (take-elapsed-times low count)
  (define (iter times n c)
    (cond ((= 0 c) (reverse times))
          ((odd? n)
           (let ((start-time (runtime)))
             (if (prime? n)
               (let ((elapsed-time (- (runtime) start-time)))
                 (iter (cons elapsed-time times) (+ n 2) (- c 1)))
               (iter times (+ n 2) c))))
          (else (iter times (+ n 1) c))))
  (iter '() low count))

(define (average seq)
  (inexact (/ (apply + seq) (length seq))))

(define (main args)
  (print (take-primes 1000 3))
  ;=> (1009 1013 1019)
  (print (take-primes 10000 3))
  ;=> (10007 10009 10037)
  (print (take-primes 100000 3))
  ;=> (100003 100019 100043)
  (print (take-primes 1000000 3))
  ;=> (1000003 1000033 1000037)
  (print (average (take-elapsed-times 1000 3)))
  ;=> 8
  (print (average (take-elapsed-times 10000 3)))
  ;=> 25
  (print (average (take-elapsed-times 100000 3)))
  ;=> 70
  (print (average (take-elapsed-times 1000000 3)))
  ;=> 200
  ; 概ね sqrt(10) 倍ずつになっている。
  )
