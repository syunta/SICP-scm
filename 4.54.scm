(load "./amb")

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
        ((require? exp) (analyze-require exp))
        ((let? exp) (analyze (let->combination exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((application? exp) (analyze-application exp))
        (else
          (error "Unknown expression type -- ANALYZE" exp))))

; require ではなく prequire とした.
(define (require? exp) (tagged-list? exp 'prequire))

(define (require-predicate exp) (cadr exp))

(define (analyze-require exp)
  (let ((pproc (analyze (require-predicate exp))))
    (lambda (env succeed fail)
      (pproc env
             (lambda (pred-value fail2)
               (if (false? pred-value)
                 (fail2)
                 (succeed 'ok fail2)))
             fail))))

(for-each simple-ambeval
          '(
            (define (test)
              (let ((t (amb 1 2 3 4 5 6 7 8)))
                (prequire (even? t))
                (prequire (< 3 t))
                t))

            (define count 0)
            (define (test2)
              (let ((t (amb 1 2 3 4 5)))
                (prequire
                  (begin
                    (set! count (+ count 1))
                    false))))
            ))

(define (main args)
  (print-ambeval '(test) 10)
  ;=>
  ; 4
  ; 6
  ; 8
  ; End of search
  (simple-ambeval '(test2))
  (print-ambeval 'count 1)
  ; 0 副作用がやり戻されて0になる
  )
