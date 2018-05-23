(load "./query")

(define uniquely-query car)

(define (singleton-stream? stream)
  (and (pair? stream)
       (stream-null? (stream-cdr stream))))

(define (uniquely-asserted unique frame-stream)
  (stream-flatmap
    (lambda (frame)
      (let ((result (qeval (uniquely-query unique)
                           (singleton-stream frame))))
        (if (singleton-stream? result)
          result
          the-empty-stream)))
    frame-stream))

(put 'unique 'qeval uniquely-asserted)

(define test1 '(unique (job ?x (computer wizard))))
(define test2 '(and (supervisor ?person ?s)
                    (unique (supervisor ?anyone ?s))))


(define (main args)
  (print-qeval test1)
  ;=> (unique (job (Bitdiddle Ben) (computer wizard)))
  (print-qeval test2)
  ;=>
  ; (and (supervisor (Cratchet Robert) (Scrooge Eben)) (unique (supervisor (Cratchet Robert) (Scrooge Eben))))
  ; (and (supervisor (Reasoner Louis) (Hacker Alyssa P)) (unique (supervisor (Reasoner Louis) (Hacker Alyssa P))))
  )
