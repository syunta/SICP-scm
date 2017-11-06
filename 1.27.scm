(load "./1.24")

(define (Carmichael? n)
  (define (iter a)
    (if (< a 0)
      #t
      (and (= (expmod a n n) a)
           (iter (- a 1)))))
  (iter (- n 1)))

(define (main args)
  (print (Carmichael? 561))
  (print (Carmichael? 1105))
  (print (Carmichael? 1729))
  (print (Carmichael? 2465))
  (print (Carmichael? 2821))
  (print (Carmichael? 6601))
  )
