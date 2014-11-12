; constructors
(define (make-queue)
  (cons '() '()))

(define (make-item content)
  (list content (cons '() '()))) ;(content (next . prev))

; selectors
(define (front-ptr queue)
  (car queue))

(define (rear-ptr queue)
  (cdr queue))

(define (content item)
  (car item))

(define (items-ptr item)
  (cadr item))

(define (next-ptr item)
  (caadr item))

(define (prev-ptr item)
  (cdadr item))

; mutators
(define (set-front-ptr! queue item)
  (set-car! queue item))

(define (set-rear-ptr! queue item)
  (set-cdr! queue item))

(define (set-next-ptr! item next)
  (set-car! (items-ptr item) next))

(define (set-prev-ptr! item prev)
  (set-cdr! (items-ptr item) prev))

; procedures
(define (empty-queue? queue)
  (null? (front-ptr queue)))

(define (front-queue queue)
  (front-ptr queue))

(define (rear-queue queue)
  (rear-ptr queue))

(define (front-insert-queue! queue content)
  (let ((new-item (make-item content)))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-item)
           (set-rear-ptr! queue new-item)
           queue)
          (else
           (set-next-ptr! new-item (front-queue queue))
           (set-prev-ptr! (front-queue queue) new-item)
           (set-front-ptr! queue new-item)
           queue))))

(define (rear-insert-queue! queue content)
  (let ((new-item (make-item content)))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-item)
           (set-rear-ptr! queue new-item)
           queue)
          (else
           (set-next-ptr! (rear-queue queue) new-item)
           (set-prev-ptr! new-item (rear-queue queue))
           (set-rear-ptr! queue new-item)
           queue))))

(define (front-delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
          (set-front-ptr! queue (next-ptr (front-queue queue)))
          (set-prev-ptr! (front-queue queue) '())
          queue)))

(define (rear-delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
          (set-rear-ptr! queue (prev-ptr (rear-queue queue)))
          (set-next-ptr! (rear-queue queue) '())
          queue)))

(define (print-queue queue)
  (define (enumerate-content item)
    (if (null? item)
      item
      (cons (content item)
            (enumerate-content (next-ptr item)))))
  (print (enumerate-content (front-queue queue))))

(define q1 (make-queue))
(rear-insert-queue! q1 'a)
(rear-insert-queue! q1 'b)
(rear-insert-queue! q1 'c)
(front-insert-queue! q1 'd)
(print-queue q1)
;=> (d a b c)
(rear-delete-queue! q1)
(front-delete-queue! q1)
(print-queue q1)
;=> (a b)
