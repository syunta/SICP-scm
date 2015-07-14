(define (=>-clause? clause) (eq? (cadr clause) '=>))
(define (cond-test clause) (car clause))
(define (cond-recipient clause) (caddr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
    'false                          ; else節なし
    (let ((first (car clauses))
          (rest (cdr clauses)))
      (if (cond-else-clause? first)
        (if (null? rest)
          (sequence->exp (cond-actions first))
          (error "ELSE clause isn't last -- COND->IF"
                 clauses))
        (if (=>-clause? first)
          (let ((test (cond-test first)))
            (make-if test
                     ;2回testしてるのでtestに副作用がある場合不適切な変換だが、make-letがないので仕方ない
                     (list (cond-recipient first) test)
                     (expand-clauses rest)))
          (make-if (cond-predicate first)
                   (sequence->exp (cond-actions first))
                   (expand-clauses rest)))))))
