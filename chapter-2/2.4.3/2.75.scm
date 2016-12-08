(add-load-path "../../lib" :relative)
(load "table")

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* r (cos a)))
          ((eq? op 'imag-part) (* r (sin a)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          (else
            (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)

(define (apply-generic op arg)
  (arg op))

(define test
  (make-from-mag-ang (sqrt 2)
                     (/ 3.14159 4)))

(define (main args)
  (print (real-part test))
  (print (imag-part test))
  (print (magnitude test))
  (print (angle test))
  )
