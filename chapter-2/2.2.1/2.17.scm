(define (last-pair seq)
  (if (null? (cdr seq))
    seq
    (last-pair (cdr seq))))

(define (main args)
  (print (last-pair (list 23 72 149 34)))
  )
