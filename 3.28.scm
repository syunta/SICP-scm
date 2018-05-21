(define (logical-or x y)
  (cond ((and (= x 1) (= y 1)) 1)
        ((and (= x 1) (= y 0)) 1)
        ((and (= x 0) (= y 1)) 1)
        ((and (= x 0) (= y 0)) 0)
        (else (error "Invalid signal" x y))))

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
            (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)
