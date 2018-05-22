(load "./stream")

(define (rand-update x)
  (let ((a 1664525) (b 1013904223) (c (expt 2 32)))
    (modulo (+ (* a x) b) c)))

(define random-init 10)

(define (rands-requested requests)
  (define (go reqs rand)
    (if (null? reqs)
      the-empty-stream
      (let ((req (stream-car (stream-car reqs))))
        (cond ((eq? req 'generate)
               (cons-stream rand
                            (go (stream-cdr reqs) (rand-update rand))))
              ((eq? req 'reset)
               (let ((new-rand-init (stream-car (stream-cdr (stream-car reqs)))))
                 (cons-stream new-rand-init
                              (go (stream-cdr reqs) (rand-update new-rand-init)))))
              (else
                (error "Unknown request --" req))))))
  (go requests random-init))

(define (main args)
  (define requests (stream '(generate)
                           '(generate)
                           '(reset 1000)
                           '(generate)
                           '(generate)
                           '(generate)))
  (display-stream
    (stream-take (rands-requested requests) 5))
  ;=>
  ;10
  ;1030549473
  ;1000
  ;2678429223
  ;4219084122
  ;2266909937
  )
