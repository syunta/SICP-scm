(load "./query")

(define (simple-stream-flatmap proc s)
  (simple-flatten (stream-map proc s)))

; a
(define (simple-flatten stream)
  (stream-map stream-car
              (stream-filter (lambda (frame) (not (null? frame))) stream)))

(define (negate operands frame-stream)
  (simple-stream-flatmap
    (lambda (frame)
      (if (stream-null? (qeval (negated-query operands)
                               (singleton-stream frame)))
        (singleton-stream frame)
        the-empty-stream))
    frame-stream))

(put 'not 'qeval negate)

(define (lisp-value call frame-stream)
  (simple-stream-flatmap
    (lambda (frame)
      (if (execute
            (instantiate
              call
              frame
              (lambda (v f)
                (error "Unknown pat var -- LISP-VALUE" v))))
        (singleton-stream frame)
        the-empty-stream))
    frame-stream))

(put 'lisp-value 'qeval lisp-value)

(define (find-assertions pattern frame)
  (simple-stream-flatmap (lambda (datum)
                           (check-an-assertion datum pattern frame))
                         (fetch-assertions pattern frame)))

(for-each (lambda (q) (qeval-add `(assert! ,q)))
          '(
            (integer 1)
            (integer 2)
            (integer 3)
            (integer 4)
            (integer 5)
            ))

(define (main args)
  (print-qeval '(integer ?x))
  ;=>
  ; (integer 5)
  ; (integer 4)
  ; (integer 3)
  ; (integer 2)
  ; (integer 1)

  ;b
  ; negate, lisp-value, check-an-assersion は、いずれも singleton-stream を返すので
  ; 差し込みしても順番は変わらない。
  )
