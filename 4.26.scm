(load "./normal-order-eval-apply")

(define (unless? exp)
  (tagged-list? exp 'unless))

(define (unless-predicate exp) (cadr exp))

(define (unless-consequent exp) (caddr exp))

(define (unless-alternative exp)
  (if (not (null? (cdddr exp)))
    (cadddr exp)
    'false))

(define (unless->if exp)
  (make-if (unless-predicate exp)
           (unless-alternative exp)
           (unless-consequent exp)))

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
        ((let? exp) (eval (let->combination exp) env))
        ((let*? exp) (eval (let*->nested-lets exp) env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((unless? exp) (eval (unless->if exp) env))
        ((and? exp) (eval-and (and-predicates exp) env))
        ((or? exp) (eval-or (or-predicates exp) env))
        ((application? exp)
         (apply (actual-value (operator exp) env)
                (operands exp)
                env))
        (else
          (error "Unknown expression type -- EVAL" exp))))

(define (main args)
  (print (actual-value
           '(begin
              (define a 10)
              (define b 20)
              (unless (= b 0)
                (/ a b)
                (begin (display "exception: returning 0")
                       0)))
           the-global-environment))
  ;=> 1/2
  ; 手続きとして使えると、高階関数に渡したり他の手続きと合成できるので有用な場面はある
  )
