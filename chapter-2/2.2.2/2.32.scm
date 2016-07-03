(define nil '())


(define (subsets s)
  (if (null? s)
    (list nil)
    (let ((rest (subsets (cdr s))))
      (append rest (map (lambda (x)
                          (cons (car s) x))
                        rest)))))

; s => (3)
; rest => (())
; next => (() (3))

; s => (2 3)
; rest => (() (3))
; next => (() (3) (2) (2 3))

; s => (1 2 3)
; rest => (() (3) (2 3))
; next => (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))

(define (main args)
  (print (subsets '(1 2 3)))
  )
