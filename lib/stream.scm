(define the-empty-stream '())

(define-syntax cons-stream
  (syntax-rules
    ()
    ((_ <a> <b>)
     (cons <a> (delay <b>)))))
