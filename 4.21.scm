(load "./eval-apply")

; a
(define fibonacci
  (lambda (n)
    ((lambda (fib)
       (fib fib n))
     (lambda (fb k)
       (cond ((= k 0) 0)
             ((= k 1) 1)
             (else (+ (fb fb (- k 1))
                      (fb fb (- k 2)))))))))

; b
(define (f x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) true (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0) false (ev? ev? od? (- n 1))))))


(define (main args)
  (print (fibonacci 10))
  ;=> 55
  (print (f 10))
  ;=> #t
  (print (f 11))
  ;=> #f
  )
