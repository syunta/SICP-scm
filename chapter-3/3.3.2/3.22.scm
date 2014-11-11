(define (make-queue)
  (let ((front-ptr '() )
        (rear-ptr '() ))
    (define (set-front-ptr! item) (set! front-ptr item))
    (define (set-rear-ptr! item) (set! rear-ptr item))
    (define (empty-queue?) (null? front-ptr))
    (define (front-queue)
      (if (empty-queue?)
        (error "FRONT called with an empty queue")
        (car front-ptr)))
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?)
               (set-front-ptr! new-pair)
               (set-rear-ptr! new-pair)
               'ok)
              (else
                (set-cdr! rear-ptr new-pair)
                (set-rear-ptr! new-pair)
                'ok))))
    (define (delete-queue!)
      (cond ((empty-queue?)
             (error "DELETE! called with an empty queue"))
            (else (set-front-ptr! (cdr front-ptr))
                  'ok)))
    (define (print-queue) (print front-ptr))
    (define (dispatch m)
      (cond ((eq? m 'front-queue) front-queue)
            ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'delete-queue!) delete-queue!)
            ((eq? m 'print-queue) print-queue)
            (else (error "Undefined operation -- MAKE-QUEUE" m))))
    dispatch))

(define (front-queue queue) ((queue 'front-queue)))

(define (insert-queue! queue item) ((queue 'insert-queue!) item))

(define (delete-queue! queue) ((queue 'delete-queue!)))

(define (print-queue queue) ((queue 'print-queue)))

(define q1 (make-queue))
(define q2 (make-queue))
(insert-queue! q1 'a)
(insert-queue! q1 'b)
(insert-queue! q2 'd)
(insert-queue! q1 'c)
(print (front-queue q1))
;=> a
(print-queue q1)
;=> (a b c)
(delete-queue! q1)
(print-queue q1)
;=> (b c)
(print-queue q2)
;=> (d)
