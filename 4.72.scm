(load "./query")

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
            (loop1 1)
            (rule (loop1 ?x)
                  (loop1 ?x))

            (loop2 2)
            (rule (loop2 ?x)
                  (loop2 ?x))

            (rule (loop ?x)
                  (or (loop1 ?x)
                      (loop2 ?x)))
            ))

(define (main args)
  (print-qeval '(loop ?x))
  ;=>
  ; (loop 1)
  ; (loop 1)
  ; (loop 1)
  ; ... 

  ; interleave-delayed を使えば,

  ;=>
  ; (loop 1)
  ; (loop 2)
  ; ...

  ; と、交互に表示される.
  )
