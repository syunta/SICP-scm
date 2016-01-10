(load "separated-analysis-excution-eval-apply")

; Analyze
(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((amb? exp) (analyze-amb exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((let? exp) (analyze (let->combination exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((application? exp) (analyze-application exp))
        (else
          (error "Unknown expression type -- ANALYZE" exp))))

; Amb
(define (amb? exp) (tagged-list? exp 'amb))
(define (amb-choices exp) (cdr exp))

(define (ambeval exp env succeed fail)
  ((analyze exp) env succeed fail))

; Analyze
(define (analyze-self-evaluating exp)
  (lambda (env succeed fail)
    (succeed exp fail)))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed fail)
      (succeed qval fail))))

(define (analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env)
             fail)))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env succeed fail)
      (succeed (make-procedure vars bproc env)
               fail))))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env succeed fail)
      (pproc env
             ;; pred-valueを得るための
             ;; 述語の評価の成功継続
             (lambda (pred-value fail2)
               (if (true? pred-value)
                 (cproc env succeed fail2)
                 (aproc env succeed fail2)))
             ;; 述語の評価の失敗継続
             fail))))

(define (analyze-sequence exps)
  (define (sequentially a b)
    (lambda (env succeed fail)
      (a env
         ;; aを呼び出す時の成功継続
         (lambda (a-value fail2)
           (b env succeed fail2))
         ;; aを呼び出す時の失敗継続
         fail)))
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
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (define-variable! var val env)
               (succeed 'ok fail2))
             fail))))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (let ((old-value
                       (lookup-variable-value var env)))
                 (set-variable-value! var val env)
                 (succeed 'ok
                          (lambda ()
                            (set-variable-value! var
                                                 old-value
                                                 env)
                            (fail2)))))
             fail))))

(define (analyze-application exp)
  (let ((pproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env succeed fail)
      (pproc env
             (lambda (proc fail2)
               (get-args aprocs
                         env
                         (lambda (args fail3)
                           (execute-application
                             proc args succeed fail3))
                         fail2))
             fail))))

(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
    (succeed '() fail)
    ((car aprocs) env
                  ;; このaprocの成功継続
                  (lambda (arg fail2)
                    (get-args (cdr aprocs)
                              env
                              ;; get-argsの再帰呼出しの
                              ;; 成功継続
                              (lambda (args fail3)
                                (succeed (cons arg args)
                                         fail3))
                              fail2))
                  fail)))

(define (execute-application proc args succeed fail)
  (cond ((primitive-procedure? proc)
         (succeed (apply-primitive-procedure proc args)
                  fail))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))
          succeed
          fail))
        (else
          (error
            "Unknown procedure type -- EXECUTE-APPLICATION"
            proc))))

(define (analyze-amb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
          (fail)
          ((car choices) env
                         succeed
                         (lambda ()
                           (try-next (cdr choices))))))
      (try-next cprocs))))

(define (driver-loop)
  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let ((input (read)))
      (if (eq? input 'try-again)
        (try-again)
        (begin
          (newline)
          (display ";;; Starting a new problem ")
          (ambeval input
                   the-global-environment
                   ;; ambeval 成功
                   (lambda (val next-alternative)
                     (announce-output output-prompt)
                     (user-print val)
                     (internal-loop next-alternative))
                   ;; ambeval 失敗
                   (lambda ()
                     (announce-output
                       ";;; There are no more values of")
                     (user-print input)
                     (driver-loop)))))))
  (internal-loop
    (lambda ()
      (newline)
      (display ";;; There is no current problem")
      (driver-loop))))

(define (simple-ambeval exp)
  (ambeval exp
           the-global-environment
           (lambda (val next-alternative))
           (lambda ())))

(for-each simple-ambeval
          '((define (require p)
              (if (not p) (amb)))
            (define (an-element-of items)
              (require (not (null? items)))
              (amb (car items) (an-element-of (cdr items))))))

(define (print-ambeval exp n)
  (let ((count n))
    (ambeval exp
             the-global-environment
             (lambda (val next-alternative)
               (cond ((< 0 count)
                      (set! count (- count 1))
                      (print val)
                      (next-alternative))
                     (else
                       (print "To be continued ..."))))
             (lambda ()
               (print "End of search")))))
