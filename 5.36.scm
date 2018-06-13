(load "./load-eceval-compiler")
(load "./compiler")

; 右から左に評価される

; 左から右に評価する版
; consではなく、appendなど使いarglを組み立てればよい
(define (construct-arglist operand-codes)
  (if (null? operand-codes)
    (make-instruction-sequence '() '(argl)
                               '((assign argl (const ()))))
    (let ((code-to-get-last-arg
            (append-instruction-sequences
              (car operand-codes)
              (make-instruction-sequence '(val) '(argl)
                                         '((assign argl (op list) (reg val)))))))
      (if (null? (cdr operand-codes))
        code-to-get-last-arg
        (preserving '(env)
                    code-to-get-last-arg
                    (code-to-get-rest-args
                      (cdr operand-codes)))))))

(define (code-to-get-rest-args operand-codes)
  (let ((code-for-next-arg
          (preserving '(argl)
                      (car operand-codes)
                      (make-instruction-sequence '(val argl) '(argl)
                                                 '((assign val (op list) (reg val))
                                                   (assign argl
                                                           (op append) (reg argl) (reg val)))))))
    (if (null? (cdr operand-codes))
      code-for-next-arg
      (preserving '(env)
                  code-for-next-arg
                  (code-to-get-rest-args (cdr operand-codes))))))

; または、consでarglを組み立ててapplyの直前に値リストをreverseする方法もある。

;(define (construct-arglist operand-codes)
;  (if (null? operand-codes)
;    (make-instruction-sequence '() '(argl)
;                               '((assign argl (const ()))))
;    (let ((code-to-get-last-arg
;            (append-instruction-sequences
;              (car operand-codes)
;              (make-instruction-sequence '(val) '(argl)
;                                         '((assign argl (op list) (reg val)))))))
;      (if (null? (cdr operand-codes))
;        code-to-get-last-arg
;        (preserving '(env)
;                    code-to-get-last-arg
;                    (code-to-get-rest-args
;                      (cdr operand-codes)))))))
;
;(define (compile-procedure-call target linkage)
;  (let ((primitive-branch (make-label 'primitive-branch))
;        (compiled-branch (make-label 'compiled-branch))
;        (after-call (make-label 'after-call)))
;    (let ((compiled-linkage
;            (if (eq? linkage 'next) after-call linkage)))
;      (append-instruction-sequences
;        (make-instruction-sequence '(proc) '()
;                                   `((assign argl (op reverse) (reg argl)) ; added
;                                     (test (op primitive-procedure?) (reg proc))
;                                     (branch (label ,primitive-branch))))
;        (parallel-instruction-sequences
;          (append-instruction-sequences
;            compiled-branch
;            (compile-proc-appl target compiled-linkage))
;          (append-instruction-sequences
;            primitive-branch
;            (end-with-linkage linkage
;                              (make-instruction-sequence '(proc argl)
;                                                         (list target)
;                                                         `((assign ,target
;                                                                   (op apply-primitive-procedure)
;                                                                   (reg proc)
;                                                                   (reg argl)))))))
;        after-call))))

;; add append
(set-cdr! (last-pair eceval-operations)
          (list (list 'reverse reverse)
                (list 'append append)))

(define (main args)
  (compile-and-go
    '(begin
       (define eval-order 0)
       (define (f x y) x)
       (f (set! eval-order 'right->left)
          (set! eval-order 'left->right))
       eval-order))
  ;=>
  ; appendを使う場合、引数リストを構成するコードは遅くなると考えられる。
  ; applyの直前にreverseなら命令数は一個増えるが、効率に影響はほぼない。
  )
