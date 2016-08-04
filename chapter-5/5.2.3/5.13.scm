(load "../../lib/regsim")

(define (make-machine ops controller-text)
  (let ((machine (make-new-machine)))
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '()))
    (let ((the-ops
            (list (list 'initialize-stack
                        (lambda () (stack 'initialize)))))
          (register-table
            (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
          (error "Multiply defined register: " name)
          (let ((new-register (list name (make-register name))))
            (set! register-table
              (cons new-register register-table))
            new-register)))
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
            (cadr val)
            (cadr (allocate-register name)))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
            'done
            (begin
              ((instruction-execution-proc (car insts)))
              (execute)))))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              ((eq? message 'registers) (map car register-table))
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

(define fib-machine
  (make-machine
    (list (list '- -) (list '< <) (list '+ +))
    '(controller
       (assign continue (label fib-done))
       fib-loop
       (test (op <) (reg n) (const 2))
       (branch (label immediate-answer))
       (save continue)
       (assign continue (label afterfib-n-1))
       (save n)
       (assign n (op -) (reg n) (const 1))
       (goto (label fib-loop))
       afterfib-n-1
       (restore n)
       (restore continue)
       (assign n (op -) (reg n) (const 2))
       (save continue)
       (assign continue (label afterfib-n-2))
       (save val)
       (goto (label fib-loop))
       afterfib-n-2
       (assign n (reg val))
       (restore val)
       (restore continue)
       (assign val
               (op +) (reg val) (reg n))
       (goto (reg continue))
       immediate-answer
       (assign val (reg n))
       (goto (reg continue))
       fib-done)))

(define (main args)
  (set-register-contents! fib-machine 'n 6)
  (start fib-machine)
  (print (get-register-contents fib-machine 'val))
  (print (fib-machine 'registers))
  )
