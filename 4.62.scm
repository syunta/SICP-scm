(load "./query")

(for-each (lambda (q) (qeval-add `(assert! ,q)))
          '(
            (rule (last-pair (?x) (?x)))
            (rule (last-pair (?v . ?x) (?y))
                  (last-pair ?x (?y)))
            ))

(define test1 '(last-pair (3) ?x))
(define test2 '(last-pair (1 2 3) ?x))
(define test3 '(last-pair (2 ?x) (3)))
(define test4 '(last-pair ?x (3)))

(define (main args)
  (print-qeval test1)
  ;=> (last-pair (3) (3))

  (print-qeval test2)
  ;=> (last-pair (1 2 3) (3))

  (print-qeval test3)
  ;=> (last-pair (2 3) (3))

  ;(print-qeval test4)
  ;=> 無限ループ
  )
