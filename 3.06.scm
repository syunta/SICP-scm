(define (rand-update x)
  (let ((seed-a 17)
        (seed-b 31)
        (seed-c 100))
    (modulo (+ (* seed-a x) seed-b) seed-c)))

(define rand-init 10) ;適当に仮実装

(define rand
  (let ((x rand-init))
    (define (dispatch m)
      (cond ((eq? 'generate m)
             (set! x (rand-update x))
             x)
            ((eq? 'reset m)
             (lambda (new-value)
               (set! x new-value)))
            (else (error "Unknown message --" m))))
    dispatch))

(print (rand 'generate))
(print (rand 'generate))
(print ((rand 'reset) 100))
(print (rand 'generate))
(print (rand 'generate))
