(load "./query")

(define (filter-query? query)
  (let ((t (type query)))
    (or (eq? 'not t)
        (eq? 'lisp-value t))))

(define (conjoin-delayed conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
    frame-stream
    (let ((cs1 (filter filter-query? conjuncts))
          (cs2 (filter (lambda (c) (not (filter-query? c))) conjuncts)))
      (conjoin cs1
               (conjoin cs2
                        frame-stream)))))

(put 'and 'qeval conjoin-delayed)

(for-each (lambda (q) (qeval-add `(assert! ,q)))
          '(
            (rule (lives-near-2 ?person-1 ?person-2)
                  (and (not (same ?person-1 ?person-2))
                       (address ?person-2 (?town . ?rest-2))
                       (address ?person-1 (?town . ?rest-1))))
            ))

(define (main args)
  (print-qeval '(lives-near ?x ?y))
  (print-qeval '(lives-near-2 ?x ?y))
  )
