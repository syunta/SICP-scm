;make-table内のassoc手続きを使うので修正が楽
;もし、globalのassocを修正しようとしたら、引数にsame-key?を追加し、assocを使っている箇所すべてを修正する必要がある

(define false #f)

(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
          (let ((record (assoc key-2 (cdr subtable))))
            (if record
              (cdr record)
              false))
          false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
          (let ((record (assoc key-2 (cdr subtable))))
            (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))))
          (set-cdr! local-table
                    (cons (list key-1
                                (cons key-2 value))
                          (cdr local-table)))))
      'ok)    
    (define (assoc key records)
      (cond ((null? records) false)
            ((same-key? key (caar records)) (car records))
            (else (assoc key (cdr records)))))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table
  (make-table (lambda (given-key expected-key)
                (= (floor given-key)
                   expected-key))))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(print (put 10 2 20))
(print (get 10.9 2.4))
;=> 20
