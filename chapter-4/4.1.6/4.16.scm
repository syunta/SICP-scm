(load "../../lib/eval-apply")

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

; b

(define internal-definition
  '(lambda '<vars>
     (define u '<e1>)
     (define v '<e2>)
     '<e3>))

(define internal-definition-body (cddr internal-definition))

(define let-assignment
  '(lambda '<vars>
     (let ((u '*unassigned*)
           (v '*unassigned*))
       (set! u '<e1>)
       (set! v '<e2>)
       '<e3>)))

(define let-assignment-body (cddr let-assignment))

(define (lambda-defines exp)
  (filter (lambda (x) (eq? 'define (car x))) exp))

(define (lambda-expressions exp)
  (filter (lambda (x) (not (eq? 'define (car x)))) exp))

(define (scan-out-defines body)
  (let ((vars (map definition-variable (lambda-defines body)))
        (vals (map definition-value (lambda-defines body))))
    (if (null? vars)
      body ;内部定義が見つからない場合、変換を行わない
      (list ;bodyは式のリスト
        (make-let (map (lambda (var) (list var ''*unassigned*))
                       vars)
                  (append (map (lambda (var val) (make-assignment var val))
                               vars vals)
                          (lambda-expressions body)))))))
; c

; procedure-bodyに組み込むデメリット
; procedure-bodyは実行頻度が高く、何度も変換を行うことは効率が悪い。
; make-procedureで変換しておくと、以後、procedure-body で変換後の構文がすぐ取れる。

; make-procedureに組み込むデメリット
; make-procedureで変換してしまうと、変換前のコードを使いたい場合に困る。
; 変換前のコードを別に保持する必要が出てくる。
; procedure-bodyで変換しておくと、変換前のコードを保持する必要はない。

; 変換前のコードを使いたい場合は特になさそうなのでmake-procedureで変換しておくと良さそう。

;(define (make-procedure parameters body env)
;  (list 'procedure parameters (scan-out-defines body) env))

(define (make-procedure parameters body env)
  (list 'procedure parameters (scan-out-defines body) env))

(define (main args)
  ;(driver-loop)
  ; (define x '*unassigned*) => ok
  ; x => Unassigned variable *unassigned*

  (print (scan-out-defines internal-definition-body))
  ;=>
  ;((let ((u *unassigned*) (v *unassigned*)) (set! u '<e1>) (set! v '<e2>) '<e3>))
  (print (equal? (scan-out-defines internal-definition-body)
                 let-assignment-body))
  ;=> #t

  (print (eval '((lambda (x)
                   (define u 1)
                   (define v 2)
                   (+ x u v))
                 10)
               the-global-environment))
  ;=> 13
  )
