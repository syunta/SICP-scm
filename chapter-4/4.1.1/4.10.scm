(load "../../lib/eval-apply")

; (let (var1 exp1 var2 exp2 ...)
;   <body>)

; letをこのように書けるよう修正する。

(define (seq-odd-order seq)
  (cond ((null? seq) '())
        ((null? (cdr seq)) (list (car seq)))
        (else (cons (car seq) (seq-odd-order (cddr seq))))))

(define (let-bindings exp) (cadr exp))

(define (let-variables bindings) (seq-odd-order bindings))

(define (let-expressions bindings) (seq-odd-order (cdr bindings)))

(define the-global-environment (setup-environment))

(define (main args)
  (driver-loop)
  ;(let (a 10 b 20 c 300)
  ;  (cons a (cons b (cons c '()))))
  ;=> (10 20 300)
  )
