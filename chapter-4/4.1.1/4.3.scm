(define (install-self-evaluating-packege)
  (put 'eval
       'self-evaluating
       (lambda (exp env) exp)))

(define (install-variable-packege)
  (put 'eval 'variable lookup-variable-value))

(define (install-quote-packege)
  (put 'eval
       'quote
       (lambda (exp env) (text-of-quotation exp))))

(define (install-assignment-packege)
  (put 'eval 'set! eval-assignment))

(define (install-definition-packege)
  (put 'eval 'define eval-assignment))

(define (install-if-packege)
  (put 'eval 'if eval-if))

(define (install-lambda-packege)
  (put 'eval
       'lambda
       (lambda (exp env)
         (make-procedure (lambda-parametaers exp)
                         (lambda-body exp)
                         env))))

(define (install-begin-packege)
  (put 'eval
       'begin
       (lambda (exp env) (eval-sequence (begin-actions exp) env))))

(define (install-cond-packege)
  (put 'eval
       'cond
       (lambda (exp env) (eval (cond->if exp) env))))

(define (install-application-packege)
  (put 'eval
       'application
       (lambda (exp env)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))))

(define (expression-type exp)
  (cond ((self-evaluating? exp) 'self-evaluating)
        ((variable? exp) 'variable)
        ((pair? exp)
         (let ((exp-type (get 'tag (car exp))))
           (if exp-type
             exp-type
             'application)))
        (else #f)))

(define (eval exp env)
  (let ((exp-type (expression-type exp)))
    (let ((proc (get 'eval exp-type)))
      (if proc
        (proc exp env)
        (error "Unknown expression type -- EVAL" exp)))))

; 問題2.73 の時と同様に、self-evaluatingとvariableはデータ主導に吸収仕切れない。
; さらに、独自に定義された手続きapplicationも吸収できない。
; condによる特別な判定をtagを取り出す部分で行うか、eval自体で行うかは変えられるが、結局は吸収仕切れない。
