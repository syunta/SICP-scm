(load "./5.41")

(define (compile-variable exp target linkage cenv)
  (let ((address (find-variable exp cenv)))
    (end-with-linkage
      linkage
      (make-instruction-sequence
        '(env) (list target)
        (if (eq? address 'not-found)
          `((assign ,target
                    ; get-global-environment で得られる大域環境から直接探しても良い
                    (op lookup-variable-value)
                    (const ,exp)
                    (reg env)))
          `((assign ,target
                    (op lexical-address-lookup)
                    (const ,address)
                    (reg env))))))))

(define (compile-assignment exp target linkage cenv)
  (let ((var (assignment-variable exp))
        (get-value-code
          (compile (assignment-value exp) 'val 'next cenv)))
    (let ((address (find-variable var cenv)))
      (end-with-linkage
        linkage
        (preserving
          '(env)
          get-value-code
          (make-instruction-sequence
            '(env val) (list target)
            (if (eq? address 'not-found)
              `((perform (op set-variable-value!)
                         (const ,var)
                         (reg val)
                         (reg env))
                (assign ,target (const ok)))
              `((perform (op lexical-address-set!)
                         (const ,address)
                         (reg val)
                         (reg env))
                (assign ,target (const ok))))))))))

; for debug
(define (compile-and-go expression)
  (let ((instructions
         (assemble (statements
                    (compile expression 'val 'return '()))
                   eceval)))
    (set! the-global-environment (setup-environment))
    (set-register-contents! eceval 'val instructions)
    (set-register-contents! eceval 'flag true)
    (start eceval)))

(set-cdr! (last-pair eceval-operations)
          (list (list 'lexical-address-lookup lexical-address-lookup)
                (list 'lexical-address-set! lexical-address-set!)))

(define (main args)
  (print-compiled-code
    (compile
      '((lambda (x y)
          (lambda (a b c d e)
            ((lambda (y z) (* x y z))
             (* a b x)
             (+ c d x))))
        3 4)
      'val
      'next
      the-empty-cenv))

  (compile-and-go
    '((lambda (x y)
        ((lambda (a b c d e)
           ((lambda (y z) (* x y z))
            (* a b x)
            (+ c d x)))
         1 2 3 4 5))
      3 4))
  ;=> 180
  )
