(add-load-path "../../lib" :relative)
(load "../../lib/load-eceval-compiler")
(load "../../lib/compiler")

(define (compile-procedure-call target linkage)
  (let ((primitive-branch (make-label 'primitive-branch))
        (compiled-branch (make-label 'compiled-branch))
        (compound-branch (make-label 'compound-branch)) ; added
        (after-call (make-label 'after-call)))
    (let ((compiled-linkage (if (eq? linkage 'next) after-call linkage))
          (compound-linkage (if (eq? linkage 'next) after-call linkage))) ; added
      (append-instruction-sequences
        (make-instruction-sequence
          '(proc) '()
          `((test (op primitive-procedure?) (reg proc))
            (branch (label ,primitive-branch))
            (test (op compound-procedure?) (reg proc)) ;
            (branch (label ,compound-branch))))        ; added
        (parallel-instruction-sequences
          (append-instruction-sequences
            compiled-branch
            (compile-proc-appl target compiled-linkage))
          (append-instruction-sequences                   ;
            compound-branch                               ;
            (compound-proc-appl target compound-linkage)) ; added
          (append-instruction-sequences
            primitive-branch
            (end-with-linkage
              linkage
              (make-instruction-sequence
                '(proc argl)
                (list target)
                `((assign ,target
                          (op apply-primitive-procedure)
                          (reg proc)
                          (reg argl)))))))
        after-call))))

(define (parallel-instruction-sequences . seqs)
  (define (parallel-2-sequences seq1 seq2)
    (make-instruction-sequence
      (list-union (registers-needed seq1)
                  (registers-needed seq2))
      (list-union (registers-modified seq1)
                  (registers-modified seq2))
      (append (statements seq1) (statements seq2))))
  (define (parallel-seq-list seqs)
    (if (null? seqs)
      (empty-instruction-sequence)
      (parallel-2-sequences (car seqs)
                            (parallel-seq-list (cdr seqs)))))
  (parallel-seq-list seqs))

(define (compound-proc-appl target linkage)
  (cond ((and (eq? target 'val) (not (eq? linkage 'return)))
         (make-instruction-sequence '(proc) all-regs
                                    `((save continue) ; compiled-applyでrestoreしたのでsaveし直す必要がある
                                      (assign continue (label ,linkage))
                                      (assign val (reg compapp))
                                      (goto (reg val)))))
        ((and (not (eq? target 'val))
              (not (eq? linkage 'return)))
         (let ((proc-return (make-label 'proc-return)))
           (make-instruction-sequence '(proc) all-regs
                                      `((save continue)
                                        (assign continue (label ,proc-return))
                                        (assign val (reg compapp))
                                        (goto (reg val))
                                        ,proc-return
                                        (assign ,target (reg val))
                                        (goto (label ,linkage))))))
        ((and (eq? target 'val) (eq? linkage 'return))
         (make-instruction-sequence '(proc continue) all-regs
                                    '((save continue)
                                      (assign val (reg compapp))
                                      (goto (reg val)))))
        ((and (not (eq? target 'val)) (eq? linkage 'return))
         (error "return linkage, target not val -- COMPILE" target))))

(define (main args)
  (print-compiled-code
    (compile
      '(define (f n)
         (g n))
      'val
      'next))
  (compile-and-go
    '(define (f n)
       (g n)))
  ;=>
  ; (define (g n) (* 10 n))
  ; => ok
  ; (f 10)
  ; => 100
  )
