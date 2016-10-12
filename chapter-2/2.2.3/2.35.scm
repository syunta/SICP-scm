(load "./2.34")

(define (count-leaves t)
  (accumulate +
              0
              (map (lambda (x)
                     (if (not (pair? x))
                       1
                       (count-leaves x)))
                   t)))

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(define (count-leaves-2 t)
  (accumulate +
              0
              (map (lambda (t) 1)
                   (enumerate-tree t))))

(define (main args)
  (print (count-leaves '(1 2 (3 4) (5 (6 (7) 8 (9))))))
  (print (count-leaves-2 '(1 2 (3 4) (5 (6 (7) 8 (9))))))
  )
