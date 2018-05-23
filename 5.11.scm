(load "./regsim")

; a

(define fib-machine
  (make-machine
    '(n val continue)
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
       ;(restore continue)
       (assign n (op -) (reg n) (const 2))
       ;(save continue)
       (assign continue (label afterfib-n-2))
       (save val)
       (goto (label fib-loop))
       afterfib-n-2
       ;(assign n (reg val)) ; nにはFib(n-2)がある
       ;(restore val)        ; valにはFib(n-1)がある
       (restore n) ; nにFib(n-1)を置き,valにFib(n-2)を置くようにする
       (restore continue)
       (assign val
               (op +) (reg val) (reg n))
       (goto (reg continue))
       immediate-answer
       (assign val (reg n))
       (goto (reg continue))
       fib-done)))

; b

(define (make-save inst machine stack pc)
  (let* ((reg-name (stack-inst-reg-name inst))
         (reg (get-register machine reg-name)))
    (lambda ()
      (push stack (get-contents reg))
      (push stack reg-name)
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let* ((reg-name (stack-inst-reg-name inst))
         (reg (get-register machine reg-name)))
    (lambda ()
      (let ((source-name (pop stack)))
        (if (eq? reg-name source-name)
          (begin
            (set-contents! reg (pop stack))
            (advance-pc pc))
          (error "Popped different register -- RESTORE"
                 reg-name '<- source-name))))))

(define machine-b
  (make-machine
    '(x y)
    '()
    '(controller
       (assign x (const 10))
       (save x)
       (assign x (const 50))
       (save x)
       (restore x)
       (restore y)
       done)))

; c

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack '()) ;changed
        (the-instruction-sequence '()))
    (let ((the-ops
            (list (list 'initialize-stack
                        (lambda () ;changed
                          (for-each (lambda (s) (s 'initialize))
                                    stack)))))
          (register-table
            (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
          (error "Multiply defined register: " name)
          (begin
            (set! stack (cons (list name (make-stack)) stack)) ;added
            (set! register-table
              (cons (list name (make-register name))
                    register-table))))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
            (cadr val)
            (error "Unknown register:" name))))
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
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

(define (stack-contents stack) (cadr stack))

(define (make-save inst machine stack pc)
  (let* ((reg-name (stack-inst-reg-name inst))
         (reg (get-register machine reg-name))
         (reg-stack (stack-contents (assoc reg-name stack))))
    (lambda ()
      (push reg-stack (get-contents reg))
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let* ((reg-name (stack-inst-reg-name inst))
         (reg (get-register machine reg-name))
         (reg-stack (stack-contents (assoc reg-name stack))))
    (lambda ()
      (set-contents! reg (pop reg-stack))
      (advance-pc pc))))

(define machine-c
  (make-machine
    '(x y z)
    '()
    '(controller
       (assign x (const 100))
       (save x)
       (restore x)
       (assign y (const 200))
       (save y)
       (restore y)
       (assign z (const 300))
       (save z)
       (restore y)
       done)))

(define (main args)
  (set-register-contents! fib-machine 'n 6)
  (start fib-machine)
  (print (get-register-contents fib-machine 'val))
  ;=> 8

  (start machine-b)
  (print (get-register-contents machine-b 'x))
  ;=> ERROR: Popped different register -- RESTORE y <- x

  (start machine-c)
  (print (get-register-contents machine-c 'x))
  (print (get-register-contents machine-c 'y))
  ;=> ERROR: Empty stack -- POP
  )
