(add-load-path "../../chapter-5" :relative)
(load "../../chapter-5/5.5.5/5.38d")
(load "./5.43")

(define (spread-arguments operands cenv)
  (let ((operand-codes
          (map (lambda (operand) (compile operand 'val 'next cenv))
               operands)))
    (let ((first-operand-code
            (append-instruction-sequences
              (car operand-codes)
              (make-instruction-sequence '(val) '(arg1)
                                         '((assign arg1 (reg val))))))
          (rest-operand-codes
            (map (lambda (code)
                   (append-instruction-sequences
                     code
                     (make-instruction-sequence '(val) '(arg2)
                                                '((assign arg2 (reg val))))))
                 (cdr operand-codes))))
      (cons first-operand-code rest-operand-codes))))

(define (compile-primitive primitive-op exp target linkage cenv)
  (let ((address (find-variable primitive-op cenv)))
    (if (eq? address 'not-found)
      (end-with-linkage
        linkage
        (construct-primitive-args primitive-op target
                                  (spread-arguments (operands exp) cenv)))
      ; 翻訳時環境に定義済みなら合成手続きとして翻訳すればよい
      (compile-application exp target linkage cenv))))

(define (compile exp target linkage cenv)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage))
        ((quoted? exp) (compile-quoted exp target linkage))
        ((variable? exp)
         (compile-variable exp target linkage cenv))
        ((assignment? exp)
         (compile-assignment exp target linkage cenv))
        ((definition? exp)
         (compile-definition exp target linkage cenv))
        ((if? exp) (compile-if exp target linkage cenv))
        ((lambda? exp) (compile-lambda exp target linkage cenv))
        ((begin? exp)
         (compile-sequence (begin-actions exp)
                           target
                           linkage
                           cenv))
        ((let? exp) (compile (let->combination exp) target linkage cenv))
        ((cond? exp) (compile (cond->if exp) target linkage cenv))
        ((primitive? exp) (compile-primitive (car exp) exp target linkage cenv)) ; added
        ((application? exp)
         (compile-application exp target linkage cenv))
        (else
          (error "Unknown expression type -- COMPILE" exp))))

(set-cdr! (last-pair eceval-operations)
          (list (list '+ +)
                (list '- -)
                (list '= =)
                (list '* *)))

(define (main args)
  (print-compiled-code
    (compile
      '((lambda (+ a b)
          (+ a b))
        - 10 5)
      'val
      'next
      the-empty-cenv))

  (compile-and-go
    '((lambda (+ a b)
        (+ a b))
      - 10 5))
  ;=> 5
  )
