(load "./1.42")

(define (repeated f n)
  (define (iter f-n count)
    (if (zero? count)
      f-n
      (iter (compose f f-n) (- count 1))))
  (iter (lambda (x) x) n))

(define (main args)
  (print ((repeated square 2) 5)) ;=> 625
  )
