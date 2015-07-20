(define (let-bindings exp) (cadr exp))

(define (let-body exp) (cddr exp))

(define (let-variables bindings)
  (map car bindings))

(define (let-expressions bindings)
  (map cadr bindings))


(define (let->combination exp)
  (let ((bindings (let-bindings exp)))
    (cons (make-lambda (let-variables bindings)
                       (let-body exp))
          (let-expressions bindings))))
