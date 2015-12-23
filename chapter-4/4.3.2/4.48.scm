(add-load-path "../../lib" :relative)
(load "amb")

(for-each simple-ambeval
          '(
            (define *unparsed* '())
            (define nouns '(noun student professor cat class))
            (define verbs '(verb studies lectures eats sleeps gets))
            (define articles '(article the The a))
            (define prepositions '(prep for to in by with))
            (define adjectives '(adjective angry))
            (define adverbs '(adverb fast very well))

            (define (parse-adverb-phrase)
              (amb (parse-word adverbs)
                   (list 'adverb-phrase
                         (parse-word adverbs)
                         (amb (parse-adjective-phrase)
                              (parse-adverb-phrase)))))

            (define (parse-adjective-phrase)
              (amb (parse-word adjectives)
                   (list 'adjective-phrase
                         (parse-word adjectives)
                         (parse-word nouns))))

            (define (parse-article-phrase)
              (amb (parse-simple-noun-phrase)
                   (list 'article-phrase
                         (parse-word articles)
                         (amb (parse-adjective-phrase)
                              (parse-adverb-phrase)))))

            (define (parse-simple-noun-phrase)
              (list 'simple-noun-phrase
                    (parse-word articles)
                    (parse-word nouns)))

            (define (parse-noun-phrase)
              (define (maybe-extend noun-phrase)
                (amb noun-phrase
                     (maybe-extend (list 'noun-phrase
                                         noun-phrase
                                         (parse-prepositional-phrase)))))
              (maybe-extend (parse-article-phrase)))

            (define (parse-verb-phrase)
              (define (maybe-extend verb-phrase)
                (amb verb-phrase
                     (list 'verb-phrase
                           verb-phrase
                           (amb (parse-adjective-phrase)
                                (parse-adverb-phrase)))
                     (list 'verb-phrase
                           verb-phrase
                           (parse-adverb-phrase)
                           (parse-prepositional-phrase))
                     (maybe-extend (list 'verb-phrase
                                         verb-phrase
                                         (parse-prepositional-phrase)))))
              (maybe-extend (parse-word verbs)))

            (define (parse-prepositional-phrase)
              (list 'prep-phrase
                    (parse-word prepositions)
                    (parse-noun-phrase)))

            (define (parse-sentence)
              (list 'sentence
                    (parse-noun-phrase)
                    (parse-verb-phrase)))

            (define (parse-word word-list)
              (require (not (null? *unparsed*)))
              (require (memq (car *unparsed*) (cdr word-list)))
              (let ((found-word (car *unparsed*)))
                (set! *unparsed* (cdr *unparsed*))
                (list (car word-list) found-word)))

            (define (parse input)
              (set! *unparsed* input)
              (let ((sent (parse-sentence)))
                (require (null? *unparsed*))
                sent))
            ))

(define (main args)
  (print-ambeval
    '(parse '(the cat eats)) 5)
  (print-ambeval
    '(parse '(the angry cat eats)) 5)
  (print-ambeval
    '(parse '(the very angry cat eats)) 5)
  (print-ambeval
    '(parse '(the very very angry cat eats)) 5)
  (print-ambeval
    '(parse '(the cat eats very well)) 5)
  (print-ambeval
    '(parse '(the cat eats fast in the class)) 5)
  (print-ambeval
    '(parse '(the cat eats with the student in the class very fast)) 5)
  (print-ambeval
    '(parse '(the professor gets angry)) 5)
  )
