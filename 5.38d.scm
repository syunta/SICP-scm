(load "./load-eceval-compiler")
(load "./compiler")

; (+ 1 2 3 4) => (+ (+ (+ 1 2) 3) 4) のように解釈する
; arg1に計算結果をためて、arg2には2個目以降の被演算子を代入していく

(define (spread-arguments operands)
  (let ((operand-codes
          (map (lambda (operand) (compile operand 'val 'next))
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

(define (compile-primitive op exp target linkage)
  (end-with-linkage
    linkage
    (construct-primitive-args op target
                              (spread-arguments (operands exp)))))

(define (construct-primitive-args op target operand-codes)
  (let ((first-operand-code (car operand-codes)))
    (preserving
      '(env)
      first-operand-code
      (construct-primitive-rest-args op target
                                     (cdr operand-codes)))))

(define (construct-primitive-rest-args op target rest-codes)
  (if (null? (cdr rest-codes))
    (preserving
      '(arg1)
      (car rest-codes)
      (make-instruction-sequence
        '(arg1 arg2) (list target)
        `((assign ,target (op ,op) (reg arg1) (reg arg2)))))
    (preserving
      '(env)
      (preserving
        '(arg1)
        (car rest-codes)
        (make-instruction-sequence
          '(arg1 arg2) '(arg1)
          `((assign arg1 (op ,op) (reg arg1) (reg arg2)))))
      (construct-primitive-rest-args op target (cdr rest-codes)))))

(define (primitive? exp)
  (any (lambda (op) (tagged-list? exp op))
       (list '+ '- '* '=)))

(define (compile exp target linkage)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage))
        ((quoted? exp) (compile-quoted exp target linkage))
        ((variable? exp)
         (compile-variable exp target linkage))
        ((assignment? exp)
         (compile-assignment exp target linkage))
        ((definition? exp)
         (compile-definition exp target linkage))
        ((if? exp) (compile-if exp target linkage))
        ((lambda? exp) (compile-lambda exp target linkage))
        ((begin? exp)
         (compile-sequence (begin-actions exp)
                           target
                           linkage))
        ((cond? exp) (compile (cond->if exp) target linkage))
        ((primitive? exp)
         (compile-primitive (car exp) exp target linkage))
        ((application? exp)
         (compile-application exp target linkage))
        (else
          (error "Unknown expression type -- COMPILE" exp))))

(define all-regs '(env proc val argl continue arg1 arg2))

(set-cdr! (last-pair eceval-operations)
          (list (list '+ +)
                (list '- -)
                (list '= =)
                (list '* *)))

(define (main args)
  (print-compiled-code
    (compile
      '(+ (+ 1 1 1) (+ 1 1) (+ 1 1))
      'val
      'next))
  (compile-and-go
    '(+ (+ 1 1 1) (+ 1 1) (+ 1 1)))
  ;(print-compiled-code
  ;  (compile
  ;    '(define (factorial n)
  ;       (if (= n 1)
  ;         1
  ;         (* (factorial (- n 1)) n)))
  ;    'val
  ;    'next))
  ;(compile-and-go
  ;  '(begin
  ;     (define (f n)
  ;       (if (= n 1)
  ;         1
  ;         (* (f (- n 1)) n)))
  ;     (+ (f 5) (f 5) (f 5))))
  )
