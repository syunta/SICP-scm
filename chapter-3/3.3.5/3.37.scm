(add-load-path "." :relative)
(load "constraint_system.scm")

(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5)) x)
      (cv 32)))

;       adder     multiplier  multiplier
;     +-------+   +-------+   +-------+
;     |       | d |       | c |       | a
; F---|s    a1|---|p    m1|---|m1    p|---9
;     |  a2   |   |  m2   |   |  m2   |
;     +-------+   +-------+   +-------+
;         |e          |           |b
;         |           |           |
;         32          C           5

(define (cv n)
  (let ((a-b-e (make-connector)))
    (constant n a-b-e)
    a-b-e))

(define (c/ a b)
  (let ((c (make-connector)))
    (multiplier c b a)
    c))

(define (c* c C)
  (let ((d (make-connector)))
    (multiplier c C d)
    d))

(define (c+ d e)
  (let ((F (make-connector)))
    (adder d e F)
    F))

(define (c- x y)
  (let ((z (make-connector)))
    (adder z y x)
    z))

(define C (make-connector))
(define F (celsius-fahrenheit-converter C))

(probe "C" C)
(probe "F" F)

; (define F (celsius-fahrenheit-converter C))
;
; F = f(C)
;
; f(x) = (+ (* (/ 9 5) x) 32)
;
; f(25) = 77
(print (+ (* (/ 9 5) 25) 32))
;=> 77

(set-value! C 25 'user)
;=>
; Probe: C = 25
; Probe: F = 77


; c- もテストしておく
(define (minus x)
  (c- (cv 40) x))

(define A (make-connector))
(define B (minus A))

(probe "A" A)
(probe "B" B)

(set-value! A 30 'user)
;=>
;=>
; Probe: A = 30
; Probe: B = 10
