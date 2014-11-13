; constructors
(define (make-deque)
  (cons '() '()))

(define (make-item content)
  (list content (cons '() '()))) ;(content (next . prev))

; selectors
(define (front-ptr deque)
  (car deque))

(define (rear-ptr deque)
  (cdr deque))

(define (content item)
  (car item))

(define (items-ptrs item)
  (cadr item))

(define (next-ptr item)
  (car (items-ptrs item)))

(define (prev-ptr item)
  (cdr (items-ptrs item)))

; mutators
(define (set-front-ptr! deque item)
  (set-car! deque item))

(define (set-rear-ptr! deque item)
  (set-cdr! deque item))

(define (set-next-ptr! item next)
  (set-car! (items-ptrs item) next))

(define (set-prev-ptr! item prev)
  (set-cdr! (items-ptrs item) prev))

; procedures
(define (empty-deque? deque)
  (null? (front-ptr deque)))

(define (front-deque deque)
  (front-ptr deque))

(define (rear-deque deque)
  (rear-ptr deque))

(define (front-insert-deque! deque content)
  (let ((new-item (make-item content)))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-item)
           (set-rear-ptr! deque new-item)
           deque)
          (else
           (set-next-ptr! new-item (front-deque deque))
           (set-prev-ptr! (front-deque deque) new-item)
           (set-front-ptr! deque new-item)
           deque))))

(define (rear-insert-deque! deque content)
  (let ((new-item (make-item content)))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-item)
           (set-rear-ptr! deque new-item)
           deque)
          (else
           (set-next-ptr! (rear-deque deque) new-item)
           (set-prev-ptr! new-item (rear-deque deque))
           (set-rear-ptr! deque new-item)
           deque))))

(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "DELETE! called with an empty deque" deque))
        (else
          (set-front-ptr! deque (next-ptr (front-deque deque)))
          (set-prev-ptr! (front-deque deque) '())
          deque)))

(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "DELETE! called with an empty deque" deque))
        (else
          (set-rear-ptr! deque (prev-ptr (rear-deque deque)))
          (set-next-ptr! (rear-deque deque) '())
          deque)))

(define (print-deque deque)
  (define (enumerate-content item)
    (if (null? item)
      item
      (cons (content item)
            (enumerate-content (next-ptr item)))))
  (print (enumerate-content (front-deque deque))))

(define q1 (make-deque))
(rear-insert-deque! q1 'a)
(rear-insert-deque! q1 'b)
(rear-insert-deque! q1 'c)
(front-insert-deque! q1 'd)
(print-deque q1)
;=> (d a b c)
(rear-delete-deque! q1)
(front-delete-deque! q1)
(print-deque q1)
;=> (a b)
