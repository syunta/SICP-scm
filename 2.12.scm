(load "./2.11")

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (percent i)
  (* 100.0 (/ (width i) (center i))))

(define (make-center-percent c p)
  (let ((w (/ (* c p) 100.0)))
    (if (> c 0)
      (make-interval (- c w) (+ c w))
      (make-interval (+ c w) (- c w)))))

(define (main args)
  (print (percent (make-interval 1 3)))
  (print (make-center-percent 2 50))
  (print (make-center-percent -2 50))
  )
