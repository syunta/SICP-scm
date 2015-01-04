(add-load-path "." :relative)
(load "constraint_system.scm")

; 2 * c = a + b

(define (averager a b c)
  (let ((x (make-connector))
        (d (make-connector)))
    (constant 2 d)
    (adder a b x)
    (multiplier c d x)))

(define a (make-connector))
(define b (make-connector))
(define c (make-connector))

(averager a b c)

(probe "a" a)
(probe "b" b)
(probe "c" c)

(set-value! a 10 'user)
(set-value! b 30 'user)

;=>
; Probe: a = 10
; Probe: b = 30
; Probe: c = 20
