(load "./amb")

(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((if-fail? exp) (analyze-if-fail exp))
        ((lambda? exp) (analyze-lambda exp))
        ((amb? exp) (analyze-amb exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((let? exp) (analyze (let->combination exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((application? exp) (analyze-application exp))
        (else
          (error "Unknown expression type -- ANALYZE" exp))))

(define (if-fail? exp)
  (tagged-list? exp 'if-fail))

(define (if-fail-consequent exp) (cadr exp))
(define (if-fail-alternative exp) (caddr exp))

(define (analyze-if-fail exp)
  (let ((cproc (analyze (if-fail-consequent exp)))
        (aproc (analyze (if-fail-alternative exp))))
    (lambda (env succeed fail)
      (cproc env
             succeed
             (lambda ()
               (aproc env succeed fail))))))


(define (main args)
  (print-ambeval
    '(if-fail (let ((x (an-element-of '(1 3 5))))
                (require (even? x))
                x)
              'all-odd)
    1)
  ;=> all-odd
  (print-ambeval
    '(if-fail (let ((x (an-element-of '(1 3 4 5 8))))
                (require (even? x))
                x)
              'all-odd)
    3)
  ;=> 4 8 all-odd
  )
