(load "./query")

(qeval-add
  '(assert! (rule (replace ?person-1 ?person-2)
                  (and (job ?person-1 ?job-1)
                       (job ?person-2 ?job-2)
                       (and (or (same ?job-1 ?job-2)
                                (can-do-job ?job-1 ?job-2))
                            (not (same ?person-1 ?person-2)))))))

; a
(define a '(replace ?who (Fect Cy D)))

; b
(define b '(and (replace ?who-1 ?who-2)
                (salary ?who-1 ?amount-1)
                (salary ?who-2 ?amount-2)
                (lisp-value > ?amount-2 ?amount-1)))

(define (main args)
  (print-qeval a)
  ;=>
  ;(replace (Bitdiddle Ben) (Fect Cy D))
  ;(replace (Hacker Alyssa P) (Fect Cy D))

  (print-qeval b)
  ;=>
  ;(and (replace (Aull DeWitt) (Warbucks Oliver)) (salary (Aull DeWitt) 25000) (salary (Warbucks Oliver) 150000) (lisp-value > 150000 25000))
  ;(and (replace (Fect Cy D) (Hacker Alyssa P)) (salary (Fect Cy D) 35000) (salary (Hacker Alyssa P) 40000) (lisp-value > 40000 35000))
  )
