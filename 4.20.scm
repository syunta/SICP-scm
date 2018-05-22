(load "./eval-apply")

; a
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (let ((frame (first-frame env)))
        (scan (frame-variables frame)
              (frame-values frame)))))
  (let ((var (env-loop env)))
    (if (not (eq? var '*unassigned*))
      var
      (error "Unassigned variable" var))))

(define (letrec? exp) (tagged-list? exp 'letrec))
(define letrec-body let-body)
(define letrec-bindings let-bindings)
(define letrec-variables let-variables)
(define letrec-expressions let-expressions)

(define (letrec->let exp)
  (let ((bindings (letrec-bindings exp)))
    (let ((vars (letrec-variables bindings))
          (vals (letrec-expressions bindings)))
      (make-let (map (lambda (var) (list var ''*unassigned*))
                     vars)
                (append (map (lambda (var val) (make-assignment var val))
                             vars vals)
                        (letrec-body exp))))))

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((let? exp) (eval (let->combination exp) env))
        ((let*? exp) (eval (let*->nested-lets exp) env))
        ((letrec? exp) (eval (letrec->let exp) env))
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

; b

; letrecを使う場合
'(define (f x)
   (letrec ((even?
              (lambda (n)
                (if (= n 0)
                  true
                  (odd? (- n 1)))))
            (odd?
              (lambda (n)
                (if (= n 0)
                  false
                  (even? (- n 1))))))
     <rest-body>))

'(define (f x)
   (let ((even? '*unassigned*)
         (odd? '*unassigned*))
     (set! even? (lambda (n)
                   (if (= n 0)
                     true
                     (odd? (- n 1)))))
     (set! odd? (lambda (n)
                  (if (= n 0)
                    false
                    (even? (- n 1)))))
     <rest-body>))

; 環境ダイアグラム
'(env
   (frame2
     (even? odd?) '*unassigned* '*unassigned*)
   (frame1
     (x) 5))
; 本体はこの環境下で評価され、
; (set! even? ...) の式の (lambda (n) ... odd? ...) の odd? は f の本体の中の odd? が参照されるので再帰呼び出しが可能
; even? も同様

; letを使う場合
'(define (f x)
   (letrec ((even?
              (lambda (n)
                (if (= n 0)
                  true
                  (odd? (- n 1)))))
            (odd?
              (lambda (n)
                (if (= n 0)
                  false
                  (even? (- n 1))))))
     <rest-body>))

; 環境ダイアグラム
'(env
   (frame2
     (even? odd?)
     (lambda (n) --- odd? ---)
     (lambda (n) --- even? ---))
   (frame1
     (x) 5))
; 本体はこの環境下で評価されるが、再帰呼び出しされる odd? や even? は本体の中ではなく、
; 本体の外(f が定義された環境)を指している
; もし外側の環境に odd? even? の定義がなければエラーになる上、あったとしても同じ内容でないと振る舞いが変わってしまう

(define (main args)
  (print (eval '(begin
                  (define (f x)
                    (letrec ((even?
                               (lambda (n)
                                 (if (= n 0)
                                   true
                                   (odd? (- n 1)))))
                             (odd?
                               (lambda (n)
                                 (if (= n 0)
                                   false
                                   (even? (- n 1))))))
                      (odd? x)))
                  (f 10))
               the-global-environment))
  ;=> #f
  )
