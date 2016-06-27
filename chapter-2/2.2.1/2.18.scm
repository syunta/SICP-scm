(define (reverse seq)
  (define (iter seq result)
    (if (null? seq)
      result
      (iter (cdr seq) (cons (car seq) result))))
  (iter seq '()))

(define (reverse-2 seq)
  (fold cons '() seq))

(define (main args)
  (print (reverse (list 1 4 9 16 25)))
  (print (reverse-2 (list 1 4 9 16 25)))
  )
