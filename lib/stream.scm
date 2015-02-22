(define the-empty-stream '())

(define stream-null? null?)

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

(define (stream-take s n)
  (if (< n 0)
    the-empty-stream
    (cons-stream (stream-car s)
                 (stream-take (stream-cdr s)
                              (- n 1)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
    'done
    (begin (proc (stream-car s))
           (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))
