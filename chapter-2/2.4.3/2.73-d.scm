(add-load-path "../.." :relative)
(load "lib/library.scm")

; getが逆になったんならputも逆にすればいい

(define (deriv ex var)
  (cond
    ((number? ex) 0)
    ((variable? ex)
     (if (same-variable? ex var) 1 0))
    (else ((get 'deriv (operator ex)) (operands ex) var))))

(define (operator ex) (car ex))

(define (operands ex) (cdr ex))

(define (variable? e)
  (symbol? e))

(define (same-variable? x y)
  (and (variable? x) (variable? y) (eq? x y)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (addend s) (cadr s))
(define (augend s) (caddr s))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (install-sum-package)
  (define (deriv-sum operands var)
    (let ((ex (apply make-sum operands)))
    (make-sum
      (deriv (addend ex) var)
      (deriv (augend ex) var))))

  (put 'deriv '+ deriv-sum)
  'done)

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

(define (install-product-package)
  (define (deriv-product operands var)
    (let ((ex (apply make-product operands)))
      (make-sum
        (make-product (multiplier ex)
                      (deriv (multiplicand ex) var))
        (make-product (deriv (multiplier ex) var)
                      (multiplicand ex)))))

  (put 'deriv '* deriv-product)
  'done)

(define (base e) (cadr e))
(define (exponent e) (caddr e))

(define (make-exponentiation x y)
  (cond ((=number? y 0) 1)
        ((=number? y 1) x)
        (else (list '** x y))))

(define (install-exponentiation-package)
  (define (deriv-exponentiation operands var)
    (let ((ex (apply make-exponentiation operands)))
      (make-product
        (exponent ex)
        (make-product (make-exponentiation (base ex)
                                           (- (exponent ex) 1))
                      (deriv (base ex) var)))))

  (put 'deriv '** deriv-exponentiation)
  'done)

(define (print-deriv seq)
  (let ((ex (car seq)) (var (cadr seq)))
    (print (deriv ex var))))

(define (main args)
  (print (install-sum-package))
  (print (install-product-package))
  (print (install-exponentiation-package))
  (map print-deriv
       (list '((** x 4) x)
             '((* x y) x)
             '((+ x 3) x)
             '((* (* x y) (+ x 3)) x))))
