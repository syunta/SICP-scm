(load "../4.4.1/4.63")

(for-each (lambda (q) (qeval-add `(assert! ,q)))
          '(
            (rule (end-of-grandson (grandson)))
            (rule (end-of-grandson (?x . ?y))
                  (end-of-grandson ?y))

            (rule ((great grandson) ?x ?y)
                  (and (grandson ?x ?gs)
                       (son ?gs ?y)))

            (rule ((great . ?rel) ?x ?y)
                  (and (son ?x ?s)
                       (?rel ?s ?y)
                       (end-of-grandson ?rel)))
            ))

; (grandson ?x ?y)
; (grandson . ())

(define test1 '((great grandson) ?g ?ggs))
(define test2 '(?relationship Adam Irad))
(define test3 '((great great great grandson) ?g ?ggs))

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
  (print-qeval test3)
  ;=>
  ;((great great great grandson) Enoch Jubal)
  ;((great great great grandson) Cain Lamech)
  ;((great great great grandson) Enoch Jabal)
  ;((great great great grandson) Adam Methushael)
  )
