(add-load-path "../../lib" :relative)
(load "query")

(for-each (lambda (q) (qeval-add `(assert! ,q)))
          '(
            (rule (?x next-to ?y in (?x ?y . ?u)))
            (rule (?x next-to ?y in (?v . ?z))
                  (?x next-to ?y in ?z))
            ))

(define test1 '(?x next-to ?y in (1 (2 3) 4)))
(define test2 '(?x next-to ?y in (2 1 3 1)))

(define (main args)
  (print-qeval test1)
  ;=>
  ;((2 3) next-to 4 in (1 (2 3) 4))
  ;(1 next-to (2 3) in (1 (2 3) 4))

  (print-qeval test2)
  ;=>
  ;(3 next-to 1 in (2 1 3 1))
  ;(2 next-to 1 in (2 1 3 1))
  ;(1 next-to 3 in (2 1 3 1))
  )
