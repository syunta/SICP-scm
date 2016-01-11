(add-load-path "../../lib" :relative)
(load "query")

(qeval-add
  '(assert! (rule (big-shot ?division ?person)
                  (or (and (job ?person (?division . ?type-1))
                           (supervisor ?person ?who)
                           (job ?who (?division-2 . ?type-2))
                           (not (same ?division-2 ?division)))
                      (and (job ?person (?division . ?type-1))
                           (not (supervisor ?person ?who)))))))

(define test '(big-shot ?division ?who))

(define (main args)
  (print-qeval test)
  ;=>
  ;(big-shot accounting (Scrooge Eben))
  ;(big-shot administration (Warbucks Oliver))
  ;(big-shot computer (Bitdiddle Ben))
  )
