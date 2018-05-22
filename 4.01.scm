(define (list-of-values-left exps env)
  (if (no-operands? exps)
    '()
    (let ((first (eval (first-operand exps) env)))
      (cons first
            (list-of-values (rest-operands exps) env)))))

(define (list-of-values-right exps env)
  (if (no-operands? exps)
    '()
    (let ((rest (list-of-values (rest-operands exps) env)))
      (cons (eval (first-operand exps) env)
            rest))))
