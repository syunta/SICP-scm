(load "./stream-lazy-list")

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp env))
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
         (apply (actual-value (operator exp) env)
                (operands exp)
                env))
        (else
          (error "Unknown expression type -- EVAL" exp))))

(define (text-of-quotation exp env)
  (let ((text (cadr exp)))
    (if (pair? text)
      (eval (text->list text) env)
      text)))

(define (text->list seq)
  (cond ((not (pair? seq)) (list 'quote seq))
        (else (list 'cons
                    (text->list (car seq))
                    (text->list (cdr seq))))))

(define (main args)
  (actual-value '(define a 'a) the-global-environment)
  (print (actual-value 'a the-global-environment))
  ;=> a

  (actual-value '(define quoted-list '(a b c d)) the-global-environment)
  (print (actual-value '(car (cdr quoted-list)) the-global-environment))
  ;=> b

  (actual-value '(define quoted-list '(a (b c))) the-global-environment)
  (print (actual-value '(car (cdr (car (cdr quoted-list)))) the-global-environment))
  ;=> c

  (actual-value '(define quoted-list '(1 2 3)) the-global-environment)
  (print (actual-value '(car (cdr quoted-list)) the-global-environment))
  ;=> 2

  (actual-value '(define quoted-list '(a (b c))) the-global-environment)
  (print (actual-value '(cdr (cdr quoted-list)) the-global-environment))
  ;=> ()
  )
