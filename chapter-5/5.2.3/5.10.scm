(load "../../lib/regsim")

(define (make-execution-procedure inst labels machine
                                  pc flag stack ops)
  (cond ((eq? (car inst) 'assign)
         (make-assign inst machine labels ops pc))
        ((eq? (car inst) 'test)
         (make-test inst machine labels ops flag pc))
        ((eq? (car inst) 'branch)
         (make-branch inst machine labels flag pc))
        ((eq? (car inst) 'goto)
         (make-goto inst machine labels pc))
        ((eq? (car inst) 'save)
         (make-save inst machine stack pc))
        ((eq? (car inst) 'restore)
         (make-restore inst machine stack pc))
        ((eq? (car inst) 'perform)
         (make-perform inst machine labels ops pc))
        ((eq? (car inst) 'swap) ; added
         (make-swap inst machine ops pc))
        (else (error "Unknown instruction type -- ASSEMBLE"
                     inst))))

; (swap (reg a) (reg b))
(define (make-swap inst machine ops pc)
  (let ((r1 (get-register machine
                          (register-exp-reg (swap-reg-1 inst))))
        (r2 (get-register machine
                          (register-exp-reg (swap-reg-2 inst)))))
    (lambda ()
      (let ((t (get-contents r1)))
        (set-contents! r1 (get-contents r2))
        (set-contents! r2 t)
        (advance-pc pc)))))

(define (swap-reg-1 swap-instruction)
  (cadr swap-instruction))

(define (swap-reg-2 swap-instruction)
  (caddr swap-instruction))

(define machine
  (make-machine
    '(a b)
    (list (list '* *))
    '(start
       (assign a (const 1))
       (assign b (const 3))
       (swap (reg a) (reg b))
       (assign a (op *) (reg a) (const 3))
       (assign b (op *) (reg b) (const 3))
       done)))

(define (main args)
  (start machine)
  (print (get-register-contents machine 'a))
  (print (get-register-contents machine 'b))
  )
