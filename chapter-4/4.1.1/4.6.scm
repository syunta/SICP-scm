(load "../../lib/eval-apply")

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
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
          (error "Unknown expression type -- EVAL" exp))))

(define (let-bindings exp) (cadr exp))

(define (let-body exp) (cddr exp))

(define (let-variables bindings)
  (map car bindings))

(define (let-expressions bindings)
  (map cadr bindings))

(define (let->combination exp)
  (let ((bindings (let-bindings exp)))
    (cons (make-lambda (let-variables bindings)
                       (let-body exp))
          (let-expressions bindings))))

(define (main args)

  (define let-expression
    '(let ((var1 exp1) (var2 exp2))
       body))

  (print (let->combination let-expression))
  ;=>
  ;((lambda (var1 var2)
  ;   body)
  ; exp1
  ; exp2)
  )
