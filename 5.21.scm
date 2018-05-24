(load "./regsim")

(define count-leaves-a
  (make-machine
    '(tree val continue)
    (list (list '+ +)
          (list 'null? null?)
          (list 'pair? pair?)
          (list 'not not)
          (list 'car car)
          (list 'cdr cdr))
    '(controller
       (assign continue (label count-leaves-done))
       count-leaves-loop
       (test (op null?) (reg tree))
       (branch (label immediate-answer-empty))
       (test (op pair?) (reg tree))
       (assign flag (op not) (reg flag))
       (branch (label immediate-answer-leaf))
       (save continue)
       (assign continue (label aftercount-leaves-car))
       (save tree)
       (assign tree (op car) (reg tree))
       (goto (label count-leaves-loop))
       aftercount-leaves-car
       (restore tree)
       (restore continue)
       (assign tree (op cdr) (reg tree))
       (save continue)
       (assign continue (label aftercount-leaves-cdr))
       (save val)
       (goto (label count-leaves-loop))
       aftercount-leaves-cdr
       (assign tree (reg val))
       (restore val)
       (restore continue)
       (assign val
               (op +) (reg val) (reg tree))
       (goto (reg continue))
       immediate-answer-empty
       (assign val (const 0))
       (goto (reg continue))
       immediate-answer-leaf
       (assign val (const 1))
       (goto (reg continue))
       count-leaves-done)))

(define count-leaves-b
  (make-machine
    '(n tree val continue)
    (list (list '+ +)
          (list 'null? null?)
          (list 'pair? pair?)
          (list 'not not)
          (list 'car car)
          (list 'cdr cdr))
    '(controller
       (assign n (const 0))
       (assign continue (label count-leaves-done))
       count-iter
       (assign continue (label count-iter-done))
       count-iter-loop
       (test (op null?) (reg tree))
       (branch (label immediate-answer-empty))
       (test (op pair?) (reg tree))
       (assign flag (op not) (reg flag))
       (branch (label immediate-answer-leaf))
       (save continue)
       (assign continue (label after-car))
       (save tree)
       (assign tree (op car) (reg tree))
       (goto (label count-iter-loop))
       after-car
       (restore tree)
       (restore continue)
       (assign tree (op cdr) (reg tree))
       (assign n (reg val))
       (goto (label count-iter-loop))
       immediate-answer-empty
       (assign val (reg n))
       (goto (reg continue))
       immediate-answer-leaf
       (assign val (op +) (reg n) (const 1))
       (goto (reg continue))
       count-iter-done
       count-leaves-done)))

(define (main args)
  (set-register-contents! count-leaves-a 'tree '(1 2 (3 (4 5)) 6 (7 8)))
  (start count-leaves-a)
  (print (get-register-contents count-leaves-a 'val))

  (set-register-contents! count-leaves-a 'tree '())
  (start count-leaves-a)
  (print (get-register-contents count-leaves-a 'val))

  (set-register-contents! count-leaves-b 'tree '(1 2 (3 (4 5)) 6 (7 8)))
  (start count-leaves-b)
  (print (get-register-contents count-leaves-b 'val))

  (set-register-contents! count-leaves-b 'tree '())
  (start count-leaves-b)
  (print (get-register-contents count-leaves-b 'val))
  )
