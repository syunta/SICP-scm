(add-load-path "../../lib" :relative)
(load "query")

; a
(define a '(supervisor ?who (Bitdiddle Ben)))

; b
(define b '(job ?who (accounting . ?type)))

; c
(define c '(address ?name (Slumerville . ?type)))

(define (main args)
  (print-qeval a)
  ;=>
  ;(supervisor (Tweakit Lem E) (Bitdiddle Ben))
  ;(supervisor (Fect Cy D) (Bitdiddle Ben))
  ;(supervisor (Hacker Alyssa P) (Bitdiddle Ben))

  (print-qeval b)
  ;=>
  ;(job (Cratchet Robert) (accounting scrivener))
  ;(job (Scrooge Eben) (accounting chief accountant))

  (print-qeval c)
  ;=>
  ;(address (Aull DeWitt) (Slumerville (Onion Square) 5))
  ;(address (Reasoner Louis) (Slumerville (Pine Tree Road) 80))
  ;(address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))
  )
