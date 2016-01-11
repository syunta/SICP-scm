(add-load-path "../../lib" :relative)
(load "query")

(qeval-add
  '(assert! (rule (big-shot ?division ?person)
                  (and (job ?person (?division . ?type))
                       (not (supervisor ?person ?who)))))) 

(define test '(big-shot ?division ?who))

(define (main args)
  (print-qeval test)
  ;=>
  ;(big-shot administration (Warbucks Oliver))
  )
