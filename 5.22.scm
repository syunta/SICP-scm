(load "./regsim")

(define append-machine
  (make-machine
    '(x y tmp val continue)
    (list (list 'null? null?)
          (list 'cons cons)
          (list 'car car)
          (list 'cdr cdr))
    '(controller
       (assign continue (label append-done))
       append-loop
       (test (op null?) (reg x))
       (branch (label base-case))
       (save continue)
       (assign tmp (op car) (reg x))
       (save tmp)
       (assign continue (label after-loop))
       (assign x (op cdr) (reg x))
       (goto (label append-loop))
       after-loop
       (restore tmp)
       (restore continue)
       (assign val (op cons) (reg tmp) (reg val))
       (goto (reg continue))
       base-case
       (assign val (reg y))
       (goto (reg continue))
       append-done)))

(define append!-machine
  (make-machine
    '(x y tmp val continue)
    (list (list 'set-cdr! set-cdr!)
          (list 'null? null?)
          (list 'cdr cdr))
    '(controller
       (save x)
       (goto (label last-pair-loop))
       last-pair-loop
       (assign tmp (op cdr) (reg x))
       (test (op null?) (reg tmp))
       (branch (label base-case))
       (assign x (op cdr) (reg x))
       (goto (label last-pair-loop))
       base-case
       (assign val (reg x))
       (restore x)
       (goto (label after-last-pair))
       after-last-pair
       (assign val (op set-cdr!) (reg val) (reg y))
       append!-done)))

(define (main args)
  (set-register-contents! append-machine 'x '(1 2 3))
  (set-register-contents! append-machine 'y '(4 5 6))
  (start append-machine)
  (print (get-register-contents append-machine 'val))
  (set-register-contents! append-machine 'x '())
  (set-register-contents! append-machine 'y '(4 5 6))
  (start append-machine)
  (print (get-register-contents append-machine 'val))

  (set-register-contents! append!-machine 'x '(1 2 3))
  (set-register-contents! append!-machine 'y '(4 5 6))
  (start append!-machine)
  (print (get-register-contents append!-machine 'x))
  )
