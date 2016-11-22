(load "./2.69")

(define rock-tree
  (generate-huffman-tree
    '((a 2) (na 16)
      (boom 1 ) (Sha 3)
      (Get 2) (yip 9)
      (job 2) (Wah 1))))

(define song
  '(Get a job
    Sha na na na na na na na na
    Get a job
    Sha na na na na na na na na
    Wah yip yip yip yip yip yip yip yip yip
    Sha boom))

(define (main args)
  (print (length (encode song rock-tree)))
  ;=> 符号化に84bit必要

  ; 8記号なので3bitで全ての単語を表せる
  (print (* 3 (length song)))
  ;=> 固定長では符号化に108bit必要
  )
