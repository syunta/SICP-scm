(load "./table")
(load "./2.56")

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp) var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

; b
(define (install-sum-package)
  (define (deriv-sum operands var)
    (let ((exp (apply make-sum operands)))
    (make-sum
      (deriv (addend exp) var)
      (deriv (augend exp) var))))
  (put 'deriv '+ deriv-sum)
  'done)

(define (install-product-package)
  (define (deriv-product operands var)
    (let ((exp (apply make-product operands)))
      (make-sum
        (make-product (multiplier exp)
                      (deriv (multiplicand exp) var))
        (make-product (deriv (multiplier exp) var)
                      (multiplicand exp)))))
  (put 'deriv '* deriv-product)
  'done)

; c
(define (install-exponentiation-package)
  (define (deriv-exponentiation operands var)
    (let ((exp (apply make-exponentiation operands)))
      (make-product
        (exponent exp)
        (make-product (make-exponentiation (base exp)
                                           (- (exponent exp) 1))
                      (deriv (base exp) var)))))
  (put 'deriv '** deriv-exponentiation)
  'done)

(define (main args)
  ; a
  ; 数値や変数はタグ付けできないので吸収できない
  (install-sum-package)
  (install-product-package)
  (install-exponentiation-package)
  (print (deriv '(+ x 4) 'x))
  ;=> 1
  (print (deriv '(* x 4) 'x))
  ;=> 4
  (print (deriv '(** x 4) 'x))
  ;=> (* 4 (** x 3))
  )
