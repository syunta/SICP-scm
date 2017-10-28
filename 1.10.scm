(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(define (f n) (A 0 n))
(define (g n) (A 1 n))
(define (h n) (A 2 n))
(define (k n) (* 5 n n))

(define (main args)
  (print (A 1 10))
  ;=> 1024
  (print (A 2 4))
  ;=> 65536
  (print (A 3 3))
  ;=> 65536
  (print (f 1)) ;=> 2
  (print (f 2)) ;=> 4
  (print (f 3)) ;=> 6
  (print (f 4)) ;=> 8
  (print (f 5)) ;=> 10
  ;=> 2n
  (print (g 1)) ;=> 2
  (print (g 2)) ;=> 4
  (print (g 3)) ;=> 8 
  (print (g 4)) ;=> 16
  (print (g 5)) ;=> 32
  ;=> 2^n
  (print (h 0)) ;=> 0
  (print (h 1)) ;=> 2
  (print (h 2)) ;=> 4
  (print (h 3)) ;=> 16
  (print (h 4)) ;=> 65536
  (print (h 5)) ;=> !?
  ; g(n) = 2^n なので、(A 1 n) = 2^n である
  ; h(n) = (A 2 n) = (A 1 (A 2 (- n 1)))
  ;=> 2^(h(n - 1))
  ;=> 2^(2^(2^(2 ...)))
  )
