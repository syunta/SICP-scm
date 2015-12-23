(add-load-path "../../lib" :relative)
(load "amb")

(for-each simple-ambeval
          '(
            (define *unparsed* '())
            (define nouns '(noun student professor cat class))
            (define verbs '(verb studies lectures eats sleeps gets))
            (define articles '(article the The a))
            (define prepositions '(prep for to in by with))
            (define adjectives '(adjective angry happy))
            (define adverbs '(adverb fast very well))
            (define conjunctions '(conjunction and but))

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

            (define (parse-sentences)
              (define (maybe-extend sentence)
                (amb sentence
                     (maybe-extend (list 'conjunction-sentence
                                         (parse-word conjunctions)
                                         (list sentence
                                               (parse-sentences))))))
              (maybe-extend (parse-sentence)))

            (define (parse-word word-list)
              (require (not (null? *unparsed*)))
              (require (memq (car *unparsed*) (cdr word-list)))
              (let ((found-word (car *unparsed*)))
                (set! *unparsed* (cdr *unparsed*))
                (list (car word-list) found-word)))

            (define (parse input)
              (set! *unparsed* input)
              (let ((sent (parse-sentences)))
                (require (null? *unparsed*))
                sent))
            ))

(define (main args)
  (for-each (lambda (sentence)
              (print-ambeval `(parse ',sentence) 5))
            '((the cat eats)
              (the angry cat eats)
              (the very angry cat eats)
              (the very very angry cat eats)
              (the cat eats very well)
              (the cat eats fast in the class)
              (the cat eats with the student in the class very fast)
              (the professor gets angry)
              (the cat eats and the professor gets angry but the student gets happy)
              ))
  )
