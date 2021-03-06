(define (make-stack)
  (let ((front-ptr '() ))
    (define (set-front-ptr! item) (set! front-ptr item))
    (define (empty-stack?) (null? front-ptr))
    (define (front-stack)
      (if (empty-stack?)
        (error "FRONT called with an empty stack")
        (car front-ptr)))
    (define (insert-stack! item)
      (let ((new-pair (cons item front-ptr)))
        (set-front-ptr! new-pair)
        'ok))
    (define (delete-stack!)
      (cond ((empty-stack?)
             (error "DELETE! called with an empty stack"))
            (else (set-front-ptr! (cdr front-ptr))
                  'ok)))
    (define (print-stack) (print front-ptr))
    (define (dispatch m)
      (cond ((eq? m 'front-stack) front-stack)
            ((eq? m 'insert-stack!) insert-stack!)
            ((eq? m 'delete-stack!) delete-stack!)
            ((eq? m 'empty-stack?) empty-stack?)
            ((eq? m 'print-stack) print-stack)
            (else (error "Undefined operation -- MAKE-QUEUE" m))))
    dispatch))

(define (empty-stack? stack) ((stack 'empty-stack?)))

(define (front-stack stack) ((stack 'front-stack)))

(define (insert-stack! stack item) ((stack 'insert-stack!) item))

(define (delete-stack! stack) ((stack 'delete-stack!)))

(define (print-stack stack) ((stack 'print-stack)))
