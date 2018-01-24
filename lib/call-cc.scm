(load "./separated-analysis-excution-eval-apply")

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
        ((call/cc? exp) (analyze-call/cc exp))
        ((let? exp) (analyze (let->combination exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((application? exp) (analyze-application exp))
        (else
          (error "Unknown expression type -- ANALYZE" exp))))

(define (call/cc? exp) (tagged-list? exp 'call/cc?))
(define (call/cc-exp exp) (cadr exp))

(define (continuation? p)
  (tagged-list? p 'continuation))

(define (make-continuation-procedure proc)
  (list 'continuation proc))

(define (continuation-implementation proc) (cadr proc))
(define (continuation-arg args) (car args))

(define (analyze-call/cc exp)
  (let ((cproc (analyze (call/cc-exp exp))))
    (lambda (env cont)
      (cproc env
             (lambda (proc)
               (execute-application proc
                                    (list (make-continuation-procedure cont))
                                    cont))))))

(define (ambeval exp env cont)
  ((analyze exp) env cont))

(define (analyze-self-evaluating exp)
  (lambda (env cont)
    (cont exp)))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env cont)
      (cont qval))))

(define (analyze-variable exp)
  (lambda (env cont)
    (cont (lookup-variable-value exp env))))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env cont)
      (cont (make-procedure vars bproc env)))))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env cont)
      (pproc env (lambda (pred-value)
                   (if (true? pred-value)
                     (cproc env cont)
                     (aproc env cont)))))))

(define (analyze-sequence exps)
  (define (sequentially a b)
    (lambda (env cont)
      (a env (lambda (a-value)
               (b env cont)))))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
      first-proc
      (loop (sequentially first-proc (car rest-procs))
            (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
      (error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env cont)
      (vproc env (lambda (val)
                   (define-variable! var val env)
                   (cont 'ok))))))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env cont)
      (vproc env (lambda (val)
                   (set-variable-value! var val env)
                   (cont 'ok))))))

(define (analyze-application exp)
  (let ((pproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env cont)
      (pproc env
             (lambda (proc)
               (get-args aprocs
                         env
                         (lambda (args)
                           (execute-application proc
                                                args
                                                cont))))))))

(define (get-args aprocs env cont)
  (if (null? aprocs)
    (cont '())
    ((car aprocs) env
                  (lambda (arg)
                    (get-args (cdr aprocs)
                              env
                              (lambda (args)
                                (cont (cons arg args))))))))

(define (execute-application proc args cont)
  (cond ((primitive-procedure? proc)
         (cont (apply-primitive-procedure proc args)))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))
          cont))
        ((continuation? proc)
         ((continuation-implementation proc) (continuation-arg args)))
        (else
          (error
            "Unknown procedure type -- EXECUTE-APPLICATION"
            proc))))

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (begin
      (newline)
      (ambeval input
               the-global-environment
               (lambda (val)
                 (announce-output output-prompt)
                 (user-print val)))
      (driver-loop))))

(driver-loop)
