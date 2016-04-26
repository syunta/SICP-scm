(load "../../lib/regsim")

(define machine
  (make-machine
    '(a)
    '()
    '(start
       (goto (label here))
       here
       (assign a (const 3))
       (goto (label there))
       here
       (assign a (const 4))
       (goto (label there))
       there
       )))

(define (extract-labels text receive)
  (if (null? text)
    (receive '() '())
    (extract-labels
      (cdr text)
      (lambda (insts labels)
        (let ((next-inst (car text)))
          (if (symbol? next-inst)
            (if (assoc next-inst labels)
              (error "Duplicated label -- EXTRACT-LABELS" next-inst)
              (receive insts
                       (cons (make-label-entry next-inst insts)
                             labels)))
            (receive (cons (make-instruction next-inst)
                           insts)
                     labels)))))))


(define (main args)
  (start machine)
  (print (get-register-contents machine 'a))
  ;=> 3
  ; extract-labels では末尾から命令列を組み立てているが、
  ; 順番は変えていないため最初の内容がセットされる。
  (make-machine
    '(a)
    '()
    '(start
       (goto (label here))
       here
       (assign a (const 3))
       (goto (label there))
       here
       (assign a (const 4))
       (goto (label there))
       there
       ))
  ;=> ERROR: Duplicated label -- EXTRACT-LABELS here
  )
