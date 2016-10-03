(add-load-path "../../lib" :relative)
(load "../../lib/load-eceval-compiler")
(load "../../lib/compiler")

(define (lexical-address-lookup fnum dnum env)
  (let ((vals (cdr (list-ref env fnum))))
    (let ((val (list-ref vals dnum)))
      (if (eq? val '*unassigned*)
        (error "LEXICAL-ADDRESS-LOOKUP - *unassigned* variable")
        val))))

(define (main args)
  (define test
    (list (make-frame '(a b c d) '(1 2 3 4))
          (make-frame '(x y) '(5 6))))

  (print (lookup-variable-value 'x test))
  (print (lexical-address-lookup 1 0 test))
  )
