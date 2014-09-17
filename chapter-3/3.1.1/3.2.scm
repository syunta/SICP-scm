(define (make-monitored f)
  (let ((c 0))
    (define (mf m)
      (cond ((eq? m 'how-many-calls?) c)
            ((eq? m 'reset-count) (set! c 0))
            (else (set! c (+ c 1))
                  (f m))))
    mf))

(define msqrt (make-monitored sqrt))

(print (msqrt 49))
(print (msqrt 225))
(print (msqrt 'how-many-calls?))
(print (msqrt 'reset-count))

(print (msqrt 144))
(print (msqrt 'how-many-calls?))
