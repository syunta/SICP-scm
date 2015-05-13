(load "../../lib/stream")

(define (rand-update x)
  (let ((a 17) (b 31) (c 100))
    (modulo (+ (* a x) b) c)))

(define random-init 10)

(define random-numbers
  (cons-stream random-init
               (stream-map rand-update random-numbers)))

(define (rands-requested requests)
  (define (go reqs rands)
    (if (null? reqs)
      the-empty-stream
      (let ((req (stream-car reqs)))
        (cond ((eq? req 'generate)
               (cons-stream (stream-car rands)
                            (go (stream-cdr reqs) (stream-cdr rands))))
              ((eq? req 'reset)
               (cons-stream (stream-car random-numbers)
                            (go (stream-cdr reqs) (stream-cdr random-numbers))))
              (else
                (error "Unknown request --" req))))))
  (go requests random-numbers))

(define (main args)
  (define requests (stream 'generate
                           'generate
                           'reset
                           'generate
                           'generate
                           'generate))
  (display-stream
    (stream-take (rands-requested requests) 5))
  ;=>
  ;10
  ;1
  ;10
  ;1
  ;48
  ;47
  )
