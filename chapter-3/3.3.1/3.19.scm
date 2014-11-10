(define (cycle? x)
  (define (go p)
    (if (not (pair? p))
      p
      (cdr p)))
  (define (iter-cycle? r t)
    (let ((r (go (go r)))
          (t (go t)))
      (cond ((not (pair? r)) #f)
            ((eq? r t) #t)
            (else (iter-cycle? r t)))))
  (iter-cycle? x x))

(define x '(a b c d))
(print (cycle? x))
;=> #f

;循環リストを作る
(set-cdr! (cdddr x) x)

(print (cycle? x))
;=> #t
