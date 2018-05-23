(load "./stream-lazy-list")

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-tag exp)
                         (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((let? exp) (eval (let->combination exp) env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((and? exp) (eval-and (and-predicates exp) env))
        ((or? exp) (eval-or (or-predicates exp) env))
        ((application? exp)
         (apply (actual-value (operator exp) env)
                (operands exp)
                env))
        (else
          (error "Unknown expression type -- EVAL" exp))))

(define (lambda-parameters exp)
  (if (symbol? (cadr exp))
    (caddr exp)
    (cadr exp)))

(define (lambda-body exp)
  (if (symbol? (cadr exp))
    (cdddr exp)
    (cddr exp)))

(define (lambda-tag exp)
  (if (symbol? (cadr exp))
    (cadr exp)
    '()))

(define (make-procedure tag parameters body env)
  (list 'procedure parameters body env tag))

(define (procedure-tag p)
  (car (cddddr p)))

(define (lazy-list? p)
  (eq? (procedure-tag p) 'cons))

(define (user-print object)
  (cond ((and (compound-procedure? object) (lazy-list? object))
         (display (lazy-list->list object)))
        ((compound-procedure? object)
         (display (list 'compound-procedure
                        (procedure-parameters object)
                        (procedure-body object)
                        '<procedure-env>)))
        (else (display object))))

(define (procedure->lambda procedure)
  (make-lambda (procedure-parameters procedure)
               (procedure-body procedure)))

(define (lazy-list->list obj)
  (if (not (compound-procedure? obj))
    obj
    (cons (lazy-list->list (actual-value (list 'car (procedure->lambda obj))
                                         (procedure-environment obj)))
          (lazy-list->list (actual-value (list 'cdr (procedure->lambda obj))
                                         (procedure-environment obj))))))

(actual-value '(define (cons x y)
                 (lambda cons (m) (m x y)))
              the-global-environment)

; 無限リストに対しては、無限リストかどうか判断するのが面倒なので無限に印字することにした。
; 他には、先頭の10要素のみ印字するなどが考えられる。

; なお、無限リストかどうか判定するには、
; (define-infinity ones (cons 1 ones))
; というような新しい構文を作り、人間が無限リストかどうかを定義する方法が考えられる。

(define (main args)
  (for-each (lambda (exp)
              (actual-value exp the-global-environment))
            '((define x (cons 1 (cons 2 (cons 3 '()))))
              (define y (cons 1 (cons 2 3)))
              (define z (cons (cons 1 (cons 2 '())) (cons 3 '())))))
  (for-each (lambda (exp)
              (user-print (actual-value exp the-global-environment)))
            '( x ;=> (1 2 3)
               y ;=> (1 2 .3)
               z ;=> ((1 2) 3)
               ))
  )
