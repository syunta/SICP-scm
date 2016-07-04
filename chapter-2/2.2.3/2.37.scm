(load "./2.36")

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (w)
         (dot-product v w))
       m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (v)
           (matrix-*-vector cols v))
         m)))

(define (main args)
  (print (dot-product '(1 2 3 4) '(5 6 7 8)))
  (print (matrix-*-vector '((1 2 3 4) (4 5 6 6) (6 7 8 9))
                          '(1 2 3 4)))
  (print (transpose '((1 2 3 4) (4 5 6 6) (6 7 8 9))))
  (print (matrix-*-matrix '((1 2 3 4) (4 5 6 6) (6 7 8 9))
                          '((1 2 3 4) (4 5 6 6) (6 7 8 9))))
  )
