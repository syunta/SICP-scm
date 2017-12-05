(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define rotate180 (compose rotate90 rotate90))
(define rotate270 (compose rotate90 (compose rotate90 rotate90)))
