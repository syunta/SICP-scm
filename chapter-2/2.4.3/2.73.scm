(add-load-path "../.." :relative)
(load "lib/library.scm")

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

(define (main args)
  (print (install-sum-package))
  (print (deriv '(+ x (+ x (+ x (+ 7 x)))) 'x)))
