(load "../../lib/regsim")

(define expt-machine-a
  (make-machine
    '(continue b n val)
    (list (list '* *) (list '- -) (list '= =))
    '((assign continue (label expt-done))
      expt-loop
      (test (op =) (reg n) (const 0))
      (branch (label base-case))
      (save continue)
      (assign n (op -) (reg n) (const 1))
      (assign continue (label after-expt))
      (goto (label expt-loop))
      after-expt
      (restore continue)
      (assign val (op *) (reg b) (reg val))
      (goto (reg continue))
      base-case
      (assign val (const 1))
      (goto (reg continue))
      expt-done)))

(define expt-machine-b
  (make-machine
    '(n b counter product)
    (list (list '* *) (list '- -) (list '= =))
    '((assign counter (reg n))
      (assign product (const 1))
      test-expt
      (test (op =) (reg counter) (const 0))
      (branch (label expt-done))
      (assign counter (op -) (reg counter) (const 1))
      (assign product (op *) (reg b) (reg product))
      (goto (label test-expt))
      expt-done)))

(define (main args)
  ; a
  (set-register-contents! expt-machine-a 'b 3)
  (set-register-contents! expt-machine-a 'n 3)
  (start expt-machine-a)
  (print (get-register-contents expt-machine-a 'val))
  ;=> 27

  ; b
  (set-register-contents! expt-machine-b 'b 4)
  (set-register-contents! expt-machine-b 'n 3)
  (start expt-machine-b)
  (print (get-register-contents expt-machine-b 'product))
  ;=> 64
  )
