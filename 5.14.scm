(load "./regsim")

(define factorial
  (make-machine
    '(n val continue)
    (list (list '- -) (list '* *) (list '= =))
    '(controller
       (assign continue (label fact-done))
       fact-loop
       (test (op =) (reg n) (const 1))
       (branch (label base-case))
       (save continue)
       (save n)
       (assign n (op -) (reg n) (const 1))
       (assign continue (label after-fact))
       (goto (label fact-loop))
       after-fact
       (restore n)
       (restore continue)
       (assign val (op *) (reg n) (reg val))
       (goto (reg continue))
       base-case
       (assign val (const 1))
       (goto (reg continue))
       fact-done
       (perform (op print-stack-statistics))
       (perform (op initialize-stack)))))

(define (main args)
  (for-each (lambda (n)
              (set-register-contents! factorial 'n n)
              (start factorial))
            (iota 20 1))
  ; (total-pushes = 0 maximum-depth = 0)
  ; (total-pushes = 2 maximum-depth = 2)
  ; (total-pushes = 4 maximum-depth = 4)
  ; (total-pushes = 6 maximum-depth = 6)
  ; (total-pushes = 8 maximum-depth = 8)
  ; (total-pushes = 10 maximum-depth = 10)
  ; (total-pushes = 12 maximum-depth = 12)
  ; (total-pushes = 14 maximum-depth = 14)
  ; (total-pushes = 16 maximum-depth = 16)
  ; (total-pushes = 18 maximum-depth = 18)
  ; (total-pushes = 20 maximum-depth = 20)
  ; (total-pushes = 22 maximum-depth = 22)
  ; (total-pushes = 24 maximum-depth = 24)
  ; (total-pushes = 26 maximum-depth = 26)
  ; (total-pushes = 28 maximum-depth = 28)
  ; (total-pushes = 30 maximum-depth = 30)
  ; (total-pushes = 32 maximum-depth = 32)
  ; (total-pushes = 34 maximum-depth = 34)
  ; (total-pushes = 36 maximum-depth = 36)
  ; (total-pushes = 38 maximum-depth = 38)

  ; 退避演算の全数と、スタックの最大深さは 2n - 2
  )
