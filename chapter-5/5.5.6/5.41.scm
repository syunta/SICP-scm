(load "./5.40")

(define (find-variable var cenv)
  (define (scan-cframe cframe dnum)
    (cond ((null? cframe) #f)
          ((eq? (car cframe) var) dnum)
          (else (scan-cframe (cdr cframe) (+ 1 dnum)))))
  (define (scan-cenv cenv fnum)
    (if (null? cenv)
      'not-found
      (let ((dnum (scan-cframe (car cenv) 0)))
        (if dnum
          (list fnum dnum)
          (scan-cenv (cdr cenv) (+ 1 fnum))))))
  (scan-cenv cenv 0))

(define (main args)
  (define test-cenv '((y z) (a b c d e) (x y)))
  (print (find-variable 'c test-cenv))
  ;=> (1 2)
  (print (find-variable 'x test-cenv))
  ;=> (2 0)
  (print (find-variable 'w test-cenv))
  ;=> not-found
  )
