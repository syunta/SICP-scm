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
     (let ((u *unassigned*)
           (v *unassigned*))
       (set! u '<e1>)
       (set! v '<e2>)
       '<e3>)))

(define let-assignment-body (cddr let-assignment))

(define (lambda-defines exp)
  (filter (lambda (x) (eq? 'define (car x))) exp))

(define (lambda-expressions exp)
  (filter (lambda (x) (not (eq? 'define (car x)))) exp))

(define (scan-out-defines exps)
  (let ((vars (map definition-variable (lambda-defines exps)))
        (vals (map definition-value (lambda-defines exps))))
    (list ;bodyは式のリスト
      (make-let (map (lambda (var) (list var '*unassigned*))
                     vars)
                (append (map (lambda (var val) (make-assignment var val))
                             vars vals)
                        (lambda-expressions exps))))))

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
  )
