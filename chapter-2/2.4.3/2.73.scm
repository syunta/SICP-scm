(define (deriv ex var)
  (cond
    ((number? ex) 0)
    ((variable? ex)
     (if (same-variable? ex var) 1 0))
    (else ((get 'deliv (operator ex)) (operands ex) var))))

(define (operator ex) (car ex))

(define (operands ex) (cdr ex))

; get？
