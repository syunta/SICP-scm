(load "../../lib/stream")

(define integral delayed-integral)

(define (RLC R L C dt)
  (lambda (vC0 iL0)
    (define vC (integral (delay dvC) vC0 dt))
    (define iL (integral (delay diL) iL0 dt))
    (define dvC (scale-stream iL (- (/ 1 C))))
    (define diL (add-streams (scale-stream vC (/ 1 L))
                             (scale-stream iL (- (/ R L)))))
    (stream-map (lambda (x y) (cons x y)) vC iL)))

(define (main args)
  (define RLC1 (RLC 1 1 0.2 0.1))
  (display-stream
    (stream-take (RLC1 10 0) 10)))
