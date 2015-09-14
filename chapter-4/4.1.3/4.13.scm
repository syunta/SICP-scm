(load "../../lib/eval-apply")

; 仕様
; unbind!は、defineと対になるようにする。
; 環境の最初のフレーム(defineの有効範囲と同じ範囲)から束縛を除去する。

; Usage
; (unbind! x)
; => 'ok

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((unbinding? exp) (eval-unbinding exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((let? exp) (eval (let->combination exp) env))
        ((let*? exp) (eval (let*->nested-lets exp) env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((and? exp) (eval-and (and-predicates exp) env))
        ((or? exp) (eval-or (or-predicates exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
          (error "Unknown expression type -- EVAL" exp))))

(define (unbinding? exp)
  (tagged-list? exp 'unbind!))

(define (eval-unbinding exp env)
  (unbind-variable! (unbind-variable exp) env)
  'ok)

(define (unbind-variable exp) (cadr exp))

(define (unbind-variable! var env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (error "Unbound variable" var))
            ((eq? var (car vars))
             (remove-binding-from-frame! var frame))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

(define (remove-binding-from-frame! var frame)
  (let loop ((prev-vars '())
             (prev-vals '())
             (vars (frame-variables frame))
             (vals (frame-values frame)))
    (let ((rest-vars (cdr vars))
          (rest-vals (cdr vals)))
      (cond ((or (null? rest-vals) (eq? var (car vars)))
             (set-car! frame (append (reverse prev-vars) rest-vars))
             (set-cdr! frame (append (reverse prev-vals) rest-vals)))
            (else
              (loop (cons (car vars) prev-vars)
                    (cons (car vals) prev-vals)
                    (cdr vars)
                    (cdr vals)))))))

(define (main args)
  (define vars '(a b c d))
  (define vals '(1 2 3 4))
  (define frame (make-frame vars vals))
  (remove-binding-from-frame! 'd frame)
  (print frame)
  ;=> ((a b c) 1 2 3)
  (remove-binding-from-frame! 'b frame)
  (print frame)
  ;=> ((a c) 1 3)
  (remove-binding-from-frame! 'a frame)
  (print frame)
  ;=> ((c) 3)
  (remove-binding-from-frame! 'c frame)
  (print frame)
  ;=> (())

  (driver-loop)
  ; (define x 10) => ok
  ; x => 10
  ; (define (not-remove-x-in-global)
  ;   (define x 20)
  ;   (unbind! x)) => ok
  ; (not-remove-x-in-global) => ok
  ; x => 10
  ; (unbind! x) => ok
  ; x => Unbound variable x
  )
