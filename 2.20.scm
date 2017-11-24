(define (same-party . args)
  (let ((pred (if (even? (car args)) even? odd?)))
    (filter pred args)))

(define (main args)
  (print (same-party 1 2 3 4 5 6 7))
  (print (same-party 2 3 4 5 6 7))
  )
