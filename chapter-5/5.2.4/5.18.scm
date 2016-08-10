(load "../../lib/regsim")

(define (make-register name)
  (let ((contents '*unassigned*)
        (trace-now #f))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value)
               (if trace-now (print name '::: contents '<- value))
               (set! contents value)))
            ((eq? message 'trace-on)
             (set! trace-now #t))
            ((eq? message 'trace-off)
             (set! trace-now #f))
            (else
              (error "Unknown request -- REGISTER" message))))
    dispatch))

(define (trace-on-register machine name)
  ((machine 'regiter-trace-on) name))

(define (trace-off-register machine name)
  ((machine 'regiter-trace-off) name))

(define (extract-labels text receive)
  (if (null? text)
    (receive '() '())
    (extract-labels (cdr text)
                    (lambda (insts labels)
                      (let ((next-inst (car text)))
                        (if (symbol? next-inst)
                          (let ((new-insts (adjoin-label next-inst insts)))
                            (receive new-insts
                                     (cons (make-label-entry next-inst new-insts)
                                           labels)))
                          (receive (cons (make-instruction next-inst)
                                         insts)
                                   labels)))))))

(define (adjoin-label label insts)
  (cond ((null? insts) insts)
        ((not (null? (cdr (car insts)))) insts)
        (else (cons (append (car insts)
                            (list label))
                    (adjoin-label label (cdr insts))))))

(define (make-instruction text)
  (cons text '()))

(define (instruction-text inst)
  (car inst))

(define (instruction-label inst)
  (cadr inst))

(define (instruction-execution-proc inst)
  (cddr inst))

(define (set-instruction-execution-proc! inst proc)
  (set-cdr! (cdr inst) proc))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        (tracing? #f)
        (instruction-count 0))
    (define (trace-on) (set! tracing? #t))
    (define (trace-off) (set! tracing? #f))
    (let ((the-ops
            (list (list 'initialize-stack
                        (lambda () (stack 'initialize)))
                  (list 'trace-on trace-on)
                  (list 'trace-off trace-off)
                  (list 'print-stack-statistics
                        (lambda () (stack 'print-statistics)))))
          (register-table
            (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
          (error "Multiply defined register: " name)
          (set! register-table
            (cons (list name (make-register name))
                  register-table)))
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
              (let ((inst (car insts)))
                (set! instruction-count (+ instruction-count 1))
                (if tracing?
                  (begin
                    (print (instruction-label inst))
                    (print (instruction-text inst))))
                ((instruction-execution-proc inst))
                (execute))))))
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
              ((eq? message 'trace-on) (trace-on))
              ((eq? message 'trace-off) (trace-off))
              ((eq? message 'instruction-count) instruction-count)
              ((eq? message 'initialize-instruction-count) (set! instruction-count 0))
              ((eq? message 'regiter-trace-on)
               (lambda (name) ((lookup-register name) 'trace-on)))
              ((eq? message 'regiter-trace-off)
               (lambda (name) ((lookup-register name) 'trace-off)))
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

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
       fact-done)))

(define (main args)
  (trace-on-register factorial 'n)
  (trace-on-register factorial 'val)
  (set-register-contents! factorial 'n 8)
  (start factorial)
  (print (get-register-contents factorial 'val))
  )
