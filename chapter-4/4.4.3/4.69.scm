(load "../4.4.1/4.63")

(for-each (lambda (q) (qeval-add `(assert! ,q)))
          '(
            (rule ((great grandson) ?x ?y)
                  (and (grandson ?x ?gs)
                       (son ?gs ?y)))
            (rule ((great . ?rel) ?x ?y)
                  (and (son ?x ?s)
                       ((?rel) ?s ?y)))
            ))

(define test1 '((great grandson) ?g ?ggs))
(define test2 '(?relationship Adam Irad))

(define (main args)
  (print-qeval test1)
  ;=>
  ;((great grandson) Mehujael Jubal)
  ;((great grandson) Irad Lamech)
  ;((great grandson) Mehujael Jabal)
  ;((great grandson) Enoch Methushael)
  ;((great grandson) Cain Mehujael)
  ;((great grandson) Adam Irad)
  (print-qeval test2)
  ;=>
  ;((great grandson) Adam Irad)
  )
