(define (for-each f seq)
  (cond ((null? seq) 'done)
        (else
          (f (car seq))
          (for-each f (cdr seq)))))

(define (main args)
  (for-each print '(57 321 88))
  )
