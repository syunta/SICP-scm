(define (fringe seq)
  (cond ((null? seq) '())
        ((not (pair? seq)) (list seq))
        (else
          (append (fringe (car seq))
                  (fringe (cdr seq))))))

(define x (list (list 1 2)
                (list 3 4)))

(define y (list (list (list 1 2) 3 (list 5 6 7))
                (list 8 9)))

(define (main args)
  (print (fringe x))
  (print (fringe (list x x)))
  (print (fringe y))
  )
