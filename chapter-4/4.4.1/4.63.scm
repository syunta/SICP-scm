(add-load-path "../../lib" :relative)
(load "query")

(for-each (lambda (q) (qeval-add `(assert! ,q)))
          '(
            (son Adam Cain)
            (son Cain Enoch)
            (son Enoch Irad)
            (son Irad Mehujael)
            (son Mehujael Methushael)
            (son Methushael Lamech)
            (wife Lamech Ada)
            (son Ada Jabal)
            (son Ada Jubal)

            (rule (grandson ?g ?s)
                  (and (son ?f ?s)
                       (son ?g ?f)))
            (rule (son ?m ?s)
                  (and (wife ?m ?w)
                       (son ?w ?s)))
            ))

;Cainの孫
(define test1 '(grandson Cain ?who))
;Lamechの息子たち
(define test2 '(son Lamech ?who))
;Methushaelの孫たち
(define test3 '(grandson Methushael ?who))

(define (main args)
  (print-qeval test1)
  ;=>
  ;(grandson Cain Irad)
  (print-qeval test2)
  ;=>
  ;(son Lamech Jubal)
  ;(son Lamech Jabal)
  (print-qeval test3)
  ;=>
  ;(grandson Methushael Jubal)
  ;(grandson Methushael Jabal)
  )
