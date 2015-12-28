(add-load-path "../../lib" :relative)
(load "amb")
(load "natural-language-parser")

(for-each simple-ambeval
          '(
            (define (parse-word word-list)
              (let ((found-word (an-element-of (cdr word-list))))
                (list (car word-list) found-word)))

            (define (parse input)
              (let ((sent (parse-sentence)))
                sent))
            ))

(define (main args)
  (print-ambeval '(parse '(the cat eats)) 3))
