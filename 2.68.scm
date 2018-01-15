(load "./2.67")

(define (encode message tree)
  (if (null? message)
    '()
    (append (encode-symbol (car message) tree)
            (encode (cdr message) tree))))

(define (encode-symbol s tree)
  (define (iter tree bits)
    (cond ((leaf? tree)
           (reverse bits))
          ((memq s (symbols (left-branch tree)))
           (iter (left-branch tree) (cons 0 bits)))
          ((memq s (symbols (right-branch tree)))
           (iter (right-branch tree) (cons 1 bits)))
          (else
            (error "ENCODE-SYMBOL -- Symbol Not Found " s))))
  (iter tree '()))

(define (main args)
  (print (encode '(A D A B B C A) sample-tree))
  ;=> (0 1 1 0 0 1 0 1 0 1 1 1 0)
  (print (equal? sample-message
                 (encode '(A D A B B C A) sample-tree)))
  )
