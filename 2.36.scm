(load "./2.35")

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
    nil
    (cons (accumulate op init (map car seqs))
          (accumulate-n op init (map cdr seqs)))))

(define (main args)
  (print (accumulate-n + 0 '((1 2 3) (4 5 6) (7 8 9) (10 11 12))))
  )
