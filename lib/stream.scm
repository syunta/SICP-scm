(define the-empty-stream '())

(define-syntax cons-stream
  (syntax-rules
    ()
    ((_ <a> <b>)
     (cons <a> (delay <b>)))))

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (stream-ref s n)
  (if (= n 0)
    (stream-car s)
    (stream-ref (stream-cdr s) (- n 1))))
