(define (cycle? x)
  (define (go x visited)
    (cond ((not (pair? x)) #f)
          ((memq x visited) #t)
          (else (go (cdr x)
                    (cons x visited)))))
  (go x '()))

(define x '(a b c))
(print (cycle? x))
;=> #f

;循環リストを作る
(set-cdr! (cddr x) x)

(print (cycle? x))
;=> #t
