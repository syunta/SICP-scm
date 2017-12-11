(define (my-equal? a b)
  (cond ((eq? a b) #t)
        ((or (not (pair? a)) (not (pair? b))) #f)
        (else (and (my-equal? (car a) (car b))
                   (my-equal? (cdr a) (cdr b))))))

(define (main args)
  (print
    (my-equal? '(this is a list) '(this is a list)))
  ;=> #t
  (print
    (my-equal? '(this is a list) '(this (is a) list)))
  ;=> #f
  (print
    (my-equal? '(this (is a) list) '(this (is a) list)))
  ;=> #t
  )
