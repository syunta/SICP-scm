(load "./load-eceval-compiler")
(load "./compiler")

(define (spread-arguments operands)
  (let ((operand-codes
          (map (lambda (operand) (compile operand 'val 'next))
               operands)))
    (let ((arg1-code
            (append-instruction-sequences
              (car operand-codes)
              (make-instruction-sequence '(val) '(arg1)
                                         '((assign arg1 (reg val))))))
          (arg2-code
            (append-instruction-sequences
              (cadr operand-codes)
              (make-instruction-sequence '(val) '(arg2)
                                         '((assign arg2 (reg val)))))))
      (list arg1-code arg2-code))))

(define (compile-primitive op exp target linkage)
  (let ((operand-codes (spread-arguments (operands exp))))
    (end-with-linkage
      linkage
      (preserving
        '(env)
        (car operand-codes)
        (preserving
          '(arg1)
          (cadr operand-codes)
          (make-instruction-sequence
            '(arg1 arg2) (list target)
            `((assign ,target (op ,op) (reg arg1) (reg arg2)))))))))

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
                (list '* *)
                (list '- -)
                (list '= =)))

(define (main args)
  ;(print-compiled-code
  ;  (compile
  ;    '(+ (+ 1 1) (+ 1 1))
  ;    'val
  ;    'next))
  ;(compile-and-go
  ;  '(+ (+ 1 1) (+ 1 1)))
  (print-compiled-code
    (compile
      '(define (factorial n)
         (if (= n 1)
           1
           (* (factorial (- n 1)) n)))
      'val
      'next))
  (compile-and-go
    '(begin
       (define (factorial n)
         (if (= n 1)
           1
           (* n (factorial (- n 1)))))
       (factorial 5)))
  )
