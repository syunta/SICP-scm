(load "./amb")

(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((permanent-assignment? exp) (analyze-permanent-assignment exp))
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

(define (permanent-assignment? exp)
  (tagged-list? exp 'permanent-set!))

(define (analyze-permanent-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (set-variable-value! var val env)
               (succeed 'ok
                        (lambda ()
                          (fail2))))
             fail))))

(for-each simple-ambeval
          '(
            (define count 0)
            (define count2 0)
            ))

(define (main args)
  (print-ambeval
    '(let ((x (an-element-of '(a b c)))
           (y (an-element-of '(a b c))))
       (permanent-set! count (+ count 1))
       (require (not (eq? x y)))
       (list x y count))
    10)
  ;=>
  ; (a b 2)
  ; (a c 3)
  ; (b a 4)
  ; (b c 6)
  ; (c a 7)
  ; (c b 8)
  ; End of serch
  (print-ambeval
    '(let ((x (an-element-of '(a b c)))
           (y (an-element-of '(a b c))))
       (set! count2 (+ count2 1))
       (require (not (eq? x y)))
       (list x y count2))
    10)
  ;=>
  ; (a b 1)
  ; (a c 1)
  ; (b a 1)
  ; (b c 1)
  ; (c a 1)
  ; (c b 1)
  ; set!を使うと副作用がやり戻されて全て1になる
  )
