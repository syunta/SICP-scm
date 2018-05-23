(load "./separated-analysis-excution-eval-apply")

; 4.6とやることはほとんど同じ。
; 導出した式を再度、analizeすること以外は同じように組み込める。

(define (analyze exp)
  (cond ((self-evaluating? exp) 
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((let? exp) (analyze (let->combination exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((application? exp) (analyze-application exp))
        (else
          (error "Unknown expression type -- ANALYZE" exp))))

(define (let? exp) (tagged-list? exp 'let))

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
  (print
    (eval '(let ((a 5)
                 (b 10))
             (+ a b))
          the-global-environment))
  ;=> 15
  )
