(add-load-path "../../lib" :relative)
(load "query")

(for-each (lambda (q) (qeval-add `(assert! ,q)))
          '(
            (rule (append-to-form () ?y ?y))
            (rule (append-to-form (?u . ?v) ?y (?u . ?z))
                  (append-to-form ?v ?y ?z))

            (rule (reverse (?x) (?x)))
            (rule (reverse (?x . ?y) (?u . ?v))
                  (and (reverse ?y ?j)
                       (append-to-form ?j (?x) (?u . ?v))))
            ))

(define (main args)
  (print-qeval '(reverse (a b c d) ?x))
  ;=> (reverse (a b c d) (d c b a))

  ;(print-qeval '(reverse ?y (a b c d)))
  ;無限ループ
  )
