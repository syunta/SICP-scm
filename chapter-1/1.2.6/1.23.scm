(load "./1.22")

(define (next n)
  (if (= n 2)
    3
    (+ n 2)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (main args)
  (print (average (take-elapsed-times 1000 3)))
  ;=> 3
  (print (average (take-elapsed-times 10000 3)))
  ;=> 10
  (print (average (take-elapsed-times 100000 3)))
  ;=> 40
  (print (average (take-elapsed-times 1000000 3)))
  ;=> 100
  ; 1.22 と比べ概ね半分になっている。
  )
