(load "./2.73")

; putも逆にする必要がある

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        (else ((get (operator exp) 'deriv) (operands exp) var))))

(define (install-sum-package)
  (define (deriv-sum operands var)
    (let ((exp (apply make-sum operands)))
      (make-sum
        (deriv (addend exp) var)
        (deriv (augend exp) var))))
  (put '+ 'deriv deriv-sum)
  'done)

(define (install-product-package)
  (define (deriv-product operands var)
    (let ((exp (apply make-product operands)))
      (make-sum
        (make-product (multiplier exp)
                      (deriv (multiplicand exp) var))
        (make-product (deriv (multiplier exp) var)
                      (multiplicand exp)))))
  (put '* 'deriv deriv-product)
  'done)

(define (install-exponentiation-package)
  (define (deriv-exponentiation operands var)
    (let ((exp (apply make-exponentiation operands)))
      (make-product
        (exponent exp)
        (make-product (make-exponentiation (base exp)
                                           (- (exponent exp) 1))
                      (deriv (base exp) var)))))
  (put '** 'deriv deriv-exponentiation)
  'done)

(define (print-deriv seq)
  (let ((exp (car seq)) (var (cadr seq)))
    (print (deriv exp var))))

(define (main args)
  (install-sum-package)
  (install-product-package)
  (install-exponentiation-package)
  (map print-deriv
       (list '((** x 4) x)
             '((* x y) x)
             '((+ x 3) x)
             '((* (* x y) (+ x 3)) x)))
  )
