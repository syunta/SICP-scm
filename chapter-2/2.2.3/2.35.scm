(load "./2.34")

(define (count-leaves t)
  (accumulate +
              0
              (map (lambda (x)
                     (if (not (pair? x))
                       1
                       (count-leaves x)))
                   t)))

(define (main args)
  (print (count-leaves '(1 2 (3 4) (5 (6 (7) 8 (9))))))
  )
