(load "./load-eceval-compiler")
(load "./compiler")

(define (get-eccompile) eccompile)

(define (start-eccompile)
  (set! the-global-environment (setup-environment))
  (start eccompile))

(define eccompile
  (make-machine
    '(exp env val proc argl continue unev arg1 arg2 compapp machine)
    (append eceval-operations
            (list (list 'get-eccompile get-eccompile)
                  (list 'compile compile)
                  (list 'statements statements)
                  (list 'assemble assemble)))
    '(
read-compile-execute-print-loop
      (assign machine (op get-eccompile))
      (perform (op initialize-stack))
      (perform
        (op prompt-for-input) (const ";;; EC-Compile input:"))
      (assign exp (op read))
      (assign env (op get-global-environment))
      (assign val (op compile) (reg exp) (const val) (const return))
      (assign val (op statements) (reg val))
      (assign val (op assemble) (reg val) (reg machine))
      (assign continue (label print-result))
      (goto (reg val))
print-result
      (perform (op print-stack-statistics))
      (perform
        (op announce-output) (const ";;; EC-Compile value:"))
      (perform (op user-print) (reg val))
      (goto (label read-compile-execute-print-loop))
      )))

(define (main args)
  (start-eccompile)
  )
