(add-load-path "../../chapter-4" :relative)
(load "./5.42")
(load "../../chapter-4/4.1.6/4.16")

(define (compile exp target linkage cenv)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage))
        ((quoted? exp) (compile-quoted exp target linkage))
        ((variable? exp)
         (compile-variable exp target linkage cenv))
        ((assignment? exp)
         (compile-assignment exp target linkage cenv))
        ((definition? exp)
         (compile-definition exp target linkage cenv))
        ((if? exp) (compile-if exp target linkage cenv))
        ((lambda? exp) (compile-lambda exp target linkage cenv))
        ((begin? exp)
         (compile-sequence (begin-actions exp)
                           target
                           linkage
                           cenv))
        ((let? exp) (compile (let->combination exp) target linkage cenv)) ; added
        ((cond? exp) (compile (cond->if exp) target linkage cenv))
        ((application? exp)
         (compile-application exp target linkage cenv))
        (else
          (error "Unknown expression type -- COMPILE" exp))))

(define (compile-lambda-body exp proc-entry cenv)
  (let ((formals (lambda-parameters exp)))
    (append-instruction-sequences
      (make-instruction-sequence
        '(env proc argl) '(env)
        `(,proc-entry
           (assign env (op compiled-procedure-env) (reg proc))
           (assign env
                   (op extend-environment)
                   (const ,formals)
                   (reg argl)
                   (reg env))))
      (compile-sequence (scan-out-defines (lambda-body exp)) ; use scan-out-defines
                        'val 'return
                        (extend-compile-time-environment formals cenv)))))

(define (main args)
  (print-compiled-code
    (compile
      '((lambda (x)
          (define u 1)
          (define v 2)
          (+ x u v))
        10)
      'val
      'next
      the-empty-cenv))

  (compile-and-go
    '((lambda (x)
        (define u 1)
        (define v 2)
        (+ x u v))
      10))
  ;=> 13
  )
