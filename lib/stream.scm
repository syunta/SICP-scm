(define the-empty-stream '())

(define-syntax cons-stream
  (syntax-rules
    ()
    ((_ <a> <b>)
     (cons <a> (delay <b>)))))

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))
