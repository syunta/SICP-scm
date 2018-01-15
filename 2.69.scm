(load "./2.68")

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge tree)
  (cond ((null? tree) '())
        ((null? (cdr tree)) (car tree))
        (else
          (let ((lower (car tree))
                (upper (cadr tree))
                (rest (cddr tree)))
            (successive-merge (adjoin-set (make-code-tree lower upper) rest))))))

(define (main args)
  (print (generate-huffman-tree '((A 8) (B 3) (C 1) (D 1) (E 1) (F 1) (G 1) (H 1))))
  (print (equal? sample-tree
                 (generate-huffman-tree '((A 4) (B 2) (C 1) (D 1)))))
  )
