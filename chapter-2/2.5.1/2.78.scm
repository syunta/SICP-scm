(load "./2.77")

(define (attach-tag type-tag contents)
  (if (number? contents)
    (list contents)
    (cons type-tag contents)))

(define (type-tag datum)
  (if (pair? datum)
    (if (number? (car datum))
      'scheme-number
      (car datum))
    (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
    (if (number? (car datum))
      (car datum)
      (cdr datum))
    (error "Bad tagged datum -- CONTENTS" datum)))

(define (install-scheme-number-package)
  (put 'add '(scheme-number scheme-number) +)
  (put 'sub '(scheme-number scheme-number) -)
  (put 'mul '(scheme-number scheme-number) *)
  (put 'div '(scheme-number scheme-number) /)
  (put 'make 'scheme-number
       (lambda (x) (attach-tag '_ x)))
  'done)

(define (main args)
  (install-polar-package)
  (install-rectangular-package)
  (install-scheme-number-package)
  (install-rational-package)
  (install-complex-package)
  (print (add (make-scheme-number 1)
              (make-scheme-number 4)))
  ;=> 5
  )
