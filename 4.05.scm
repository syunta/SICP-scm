(load "./eval-apply")

(define (=>-clause? clause) (eq? (cadr clause) '=>))
(define (cond-test clause) (car clause))
(define (cond-recipient clause) (caddr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
    'false ; else節なし
    (let ((first (car clauses))
          (rest (cdr clauses)))
      (if (cond-else-clause? first)
        (if (null? rest)
          (sequence->exp (cond-actions first))
          (error "ELSE clause isn't last -- COND->IF"
                 clauses))
        (if (=>-clause? first)
          ; clause で _x を使っていた場合、変数が衝突してしまう
          (list (make-lambda '_x
                             (list (make-if '_x
                                            (list (cond-recipient first) '_x)
                                            (expand-clauses rest))))
                (cond-test first))
          (make-if (cond-predicate first)
                   (sequence->exp (cond-actions first))
                   (expand-clauses rest)))))))

(define (main args)

  (define cond-expression
    '(cond ((assoc 'b '((a 1) (b 2))) => cadr)
           (else false)))

  (print (cond->if cond-expression))
  ;=>
  ;((lambda _x
  ;   (if _x
  ;     (cadr _x)
  ;     false))
  ; (assoc 'b '((a 1) (b 2))))
  )
