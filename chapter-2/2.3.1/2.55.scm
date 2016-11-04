(define (main args)
  (print (car ''abracadabra))
  ;=> quote

  ; ''abracadabra
  ; (quote (quote abracadabra))
  ; 外側の quote が (quote abracadabra) をそのまま返す
  )
