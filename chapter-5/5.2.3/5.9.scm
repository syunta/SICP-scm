(load "../../lib/regsim")

(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
        (operands (operation-exp-operands exp)))
    (if (null? (filter label-exp? operands))
      (let ((aprocs
              (map (lambda (e)
                     (make-primitive-exp e machine labels))
                   operands)))
        (lambda ()
          (apply op (map (lambda (p) (p)) aprocs))))
      (error "Label is invalid argument -- MAKE-OPERATION-EXP" exp))))

(define machine
  (make-machine
    '(a b)
    (list (list '+ +))
    '(start
       (assign a (const 1))
       (assign b (op +) (label 2) (reg a))
       done)))

(define (main args)
  (start machine)
  (print (get-register-contents machine 'b))
  )
