(use gauche.sequence)
(load "../4.3.2/4.49")

(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((amb? exp) (analyze-amb exp))
        ((ramb? exp) (analyze-ramb exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((let? exp) (analyze (let->combination exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((application? exp) (analyze-application exp))
        (else
          (error "Unknown expression type -- ANALYZE" exp))))

(define (ramb? exp) (tagged-list? exp 'ramb))

(define (analyze-ramb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
          (fail)
          ((car choices) env
                         succeed
                         (lambda ()
                           (try-next (cdr choices))))))
      (try-next (shuffle cprocs)))))

(for-each simple-ambeval
          '(
            (define (parse-noun-phrase)
              (define (maybe-extend noun-phrase)
                (ramb noun-phrase
                      (maybe-extend (list 'noun-phrase
                                          noun-phrase
                                          (parse-prepositional-phrase)))))
              (maybe-extend (parse-simple-noun-phrase)))

            (define (parse-verb-phrase)
              (define (maybe-extend verb-phrase)
                (ramb verb-phrase
                      (maybe-extend (list 'verb-phrase
                                          verb-phrase
                                          (parse-prepositional-phrase)))))
              (maybe-extend (parse-word verbs)))

            (define (ran-element-of items)
              (require (not (null? items)))
              (ramb (car items) (ran-element-of (cdr items))))

            (define (parse-word word-list)
              (let ((found-word (ran-element-of (cdr word-list))))
                (list (car word-list) found-word)))
            ))

(define (main args)
  (print-ambeval '(ramb 1 2 3 4 5 6 7 8 9 10) 10)
  (print-ambeval '(parse '(the cat eats)) 3))
