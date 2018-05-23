(load "./query")

(for-each qeval-add '((assert! (meeting accounting (Monday 9am)))
                      (assert! (meeting administration (Monday 10am)))
                      (assert! (meeting computer (Wednesday 3pm)))
                      (assert! (meeting administration (Friday 1pm)))
                      (assert! (meeting whole-company (Wednesday 4pm)))))

; a
(define a '(meeting ?division (Friday ?time)))

; b
(qeval-add 
  '(assert! (rule (meeting-time ?person ?day-and-time)
                  (or (meeting whole-company ?day-and-time)
                      (and (job ?person (?division . ?type))
                           (meeting ?division ?day-and-time))))))

; c
(define c '(meeting-time (Hacker Alyssa P) (Wednesday ?time)))

(define (main args)
  (print-qeval a)
  ;=>
  ;(meeting administration (Friday 1pm))
  (print-qeval c)
  ;=>
  ;(meeting-time (Hacker Alyssa P) (Wednesday 4pm))
  ;(meeting-time (Hacker Alyssa P) (Wednesday 3pm))
  )
