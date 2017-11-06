(load "./1.22")
(use srfi-27)

(define true #t)
(define false #f)

(define (random n)
  (random-integer n))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
          (remainder (* base (expmod base (- exp 1) m))
                     m))))        

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 5)
    (report-prime (- (runtime) start-time))))

(define (take-primes low count)
  (define (iter primes n c)
    (cond ((= 0 c) (reverse primes))
          ((odd? n)
           (if (fast-prime? n 5)
             (iter (cons n primes) (+ n 2) (- c 1))
             (iter primes (+ n 2) c)))
          (else (iter primes (+ n 1) c))))
  (iter '() low count))

(define (take-elapsed-times low count)
  (define (iter times n c)
    (cond ((= 0 c) (reverse times))
          ((odd? n)
           (let ((start-time (runtime)))
             (if (fast-prime? n 1000)
               (let ((elapsed-time (- (runtime) start-time)))
                 (iter (cons elapsed-time times) (+ n 2) (- c 1)))
               (iter times (+ n 2) c))))
          (else (iter times (+ n 1) c))))
  (iter '() low count))

(define (main args)
  (print (take-primes 1000 3))
  ;=> (1009 1013 1019)
  (print (take-primes 10000 3))
  ;=> (10007 10009 10037)
  (print (take-primes 100000 3))
  ;=> (100003 100019 100043)
  (print (take-primes 1000000 3))
  ;=> (1000003 1000033 1000037)

  ; Θ(log_2 n)の増加なので、
  ; 1000000近くの素数をテストする時間は1000近くの素数をテストする時間と比べ約2倍と予想する。
  (print (average (take-elapsed-times 1000 3)))
  ;=> 3893
  (print (average (take-elapsed-times 1000000 3)))
  ;=> 7776
  ; 概ね2倍になっている。
  )
