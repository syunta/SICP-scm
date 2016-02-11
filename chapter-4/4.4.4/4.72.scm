(add-load-path "../../lib" :relative)
(load "query")

(define (disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
    the-empty-stream
    (stream-append-delayed
      (qeval (first-disjunct disjuncts) frame-stream)
      (delay (disjoin (rest-disjuncts disjuncts)
                      frame-stream)))))

(put 'or 'qeval disjoin)

(define (flatten-stream stream)
  (if (stream-null? stream)
    the-empty-stream
    (stream-append-delayed
      (stream-car stream)
      (delay (flatten-stream (stream-cdr stream))))))

(for-each (lambda (q) (qeval-add `(assert! ,q)))
          '(
            (loop1 1 1)
            (rule (loop1 ?x ?y)
                  (loop1 ?x ?y))

            (loop2 2 2)
            (rule (loop2 ?x ?y)
                  (loop2 ?x ?y))

            (rule (loop ?x ?y)
                  (or (loop1 ?x ?y)
                      (loop2 ?x ?y)))
            ))

(define (main args)
  (print-qeval '(loop ?x ?y))
  ;=>
  ; (loop 1 1)
  ; (loop 1 1)
  ; (loop 1 1)
  ; ... 

  ; interleave-delayed を使えば,

  ;=>
  ; (loop 1 1)
  ; (loop 2 2)
  ; ...

  ; と、交互に表示される.
  )
