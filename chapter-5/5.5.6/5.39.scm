(add-load-path "../../lib" :relative)
(load "../../lib/load-eceval-compiler")
(load "../../lib/compiler")

(define (make-address fnum dnum) (list fnum dnum))
(define (frame-number address) (car address))
(define (displacement-number address) (cadr address))

(define (lexical-address-lookup address env)
  (let ((vals (cdr (list-ref env (frame-number address)))))
    (let ((val (list-ref vals (displacement-number address))))
      (if (eq? val '*unassigned*)
        (error "LEXICAL-ADDRESS-LOOKUP - *unassigned* variable")
        val))))

(define (lexical-address-set! address val env)
  (let ((vals (cdr (list-ref env (frame-number address)))))
    (list-set! vals (displacement-number address) val)))

(define (main args)
  (define test
    (list (make-frame '(a b c d) '(1 2 3 4))
          (make-frame '(x y) '(5 6))))

  (print (lookup-variable-value 'x test))
  (print (lexical-address-lookup (make-address 1 0) test))

  (lexical-address-set! (make-address 1 0) 10 test)
  (print (lexical-address-lookup (make-address 1 0) test))
  )
