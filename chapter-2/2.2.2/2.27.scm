(define (deep-reverse tree)
  (define (iter result seq)
    (cond ((null? seq) result)
          ((not (pair? seq)) seq)
          (else
             (iter (cons (deep-reverse (car seq)) result)
                   (cdr seq)))))
  (iter '() tree))

(define x (list (list 1 2)
                (list 3 4)))

(define y (list (list 1 2)
                (list (list 3 4 5 6)
                      7)
                8 9))

(define (main args)
  (print (reverse x))
  ;=> ((3 4) (1 2))
  (print (deep-reverse x))
  ;=> ((4 3) (2 1))
  (print (deep-reverse y))
  ;=> (9 8 (7 (6 5 4 3)) (2 1))
  )
