(load "./2.37")

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
      result
      (iter (op result (car rest))
            (cdr rest))))
  (iter initial sequence))

(define (main args)
  (print (fold-right / 1 (list 1 2 3)))
  ;=> (1/(2/(3/1))) -> 3/2
  (print (fold-left / 1 (list 1 2 3)))
  ;=> ((1/1)/2)/3) -> 1/6
  (print (fold-right list nil (list 1 2 3)))
  ;=> (1 (2 (3 nil)))
  (print (fold-left list nil (list 1 2 3)))
  ;=> (((nil 1) 2) 3)
  ;
  ; opが満たすべき性質は、*や+のように引数の順番が変わっても結果が変わらないこと
  )
