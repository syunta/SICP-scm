(define (rand-update x) ;適当に仮実装
  (+ x 3))

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
(print ((rand 'reset) 100))
(print (rand 'generate))
