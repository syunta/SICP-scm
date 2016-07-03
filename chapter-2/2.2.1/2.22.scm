(define nil '())

(define (square-list-1 items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things) 
            (cons (square (car things))
                  answer))))
  (iter items nil))

(define (square-list-2 items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things)
            (cons answer
                  (square (car things))))))
  (iter items nil))

(define (main args)
  (print (square-list-1 '(1 2 3 4)))
  ; (cons 16 (cons 9 (cons 4 (cons 1 nil))))
  ; こういうプロセスなので逆順になる

  (print (square-list-2 '(1 2 3 4)))
  ; (cons (cons (cons (cons nil 1) 4) 9) 16)
  ; こういうプロセスなので動かない
  )
