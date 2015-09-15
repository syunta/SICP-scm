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

(define (main args)
  (driver-loop)
  ; (define x '*unassigned*) => ok
  ; x => Unassigned variable *unassigned*
  )
