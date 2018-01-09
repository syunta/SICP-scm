(load "./2.61")

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
          (let ((x (car set1)) (y (car set2)))
            (cond ((= x y)
                   (cons x (union-set (cdr set1) (cdr set2))) )
                  ((< x y)
                   (cons x (union-set (cdr set1) set2))) 
                  ((> x y)
                   (cons y (union-set set1 (cdr set2))))))))) 


(define (main args)
  (print (union-set '(1 3 4) '(2 3 4 5 6)))
  ;=> (1 2 3 4 5 6)
  )
