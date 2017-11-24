(define (square-list items)
  (if (null? items)
    '()
    (cons (square (car items)) (square-list (cdr items)))))

(define (square-list-2 items)
  (map square items))

(define (main args)
  (print (square-list '(1 2 3 4)))
  (print (square-list-2 '(1 2 3 4)))
  )
