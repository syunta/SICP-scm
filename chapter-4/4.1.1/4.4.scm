(load "../../lib/eval-apply")

(define (and? exp) (tagged-list? exp 'and))
(define (or? exp) (tagged-list? exp 'or))
(define (and-predicates exp) (cdr exp))
(define (or-predicates exp) (cdr exp))
(define (first-predicate seq) (car seq))
(define (rest-predicates seq) (cdr seq))

(define (eval-and exps env)
  (define (go exps last-result)
    (if (null? exps)
      last-result
      (let ((value (eval (first-predicate exps) env)))
        (if (not (true? value))
          false
          (go (rest-predicates exps) value)))))
  (go exps true))

; exp が _x を使っていた場合、変数の衝突が起きてしまう
(define (and->if exp)
  (define (go predicates result)
    (if (null? predicates)
      result
      (list (make-lambda '_x
                         (list (make-if '_x
                                        (go (rest-predicates predicates) '_x)
                                        false)))
            (first-predicate predicates))))
  (go (and-predicates exp) true))

(define (eval-or exps env)
  (if (null? exps)
    false
    (let ((value (eval (first-predicate exps) env)))
      (if (true? value)
        value
        (eval-or (rest-predicates exps) env)))))

; exp が _x を使っていた場合、変数の衝突が起きてしまう
(define (or->if exp)
  (define (go predicates)
    (if (null? predicates)
      'false
      (list (make-lambda '_x
                         (list (make-if '_x
                                        '_x
                                        (go (rest-predicates predicates)))))
            (first-predicate predicates))))
  (go (or-predicates exp)))

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((and? exp) (eval-and (and-predicates exp) env))
        ;((and? exp) (eval (and->if exp) env))
        ((or? exp) (eval-or (or-predicates exp) env))
        ;((or? exp) (eval (or->if exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
          (error "Unknown expression type -- EVAL" exp))))

(define (main args)

  (define and-expression
    '(and (not (null? x)) (number? (car x)) (< 0 (car x))))

  (define or-expression
    '(or (car x) (cadr x) (caddr x)))

  (print (and->if and-expression))
  ;=>
  ;((lambda _x
  ;   (if _x
  ;     ((lambda _x
  ;        (if _x
  ;          ((lambda _x
  ;             (if _x
  ;               _x
  ;               false))
  ;           (< 0 (car x)))
  ;          false))
  ;      (number? (car x)))
  ;     false))
  ; (not (null? x)))

  (print (or->if or-expression))
  ;=>
  ;((lambda _x
  ;   (if _x
  ;     _x
  ;     ((lambda _x
  ;        (if _x
  ;          _x
  ;          ((lambda _x
  ;             (if _x
  ;               _x
  ;               false))
  ;           (caddr x))))
  ;      (cadr x))))
  ; (car x))
  )
