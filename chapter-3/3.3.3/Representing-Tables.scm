(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
      (cdr record)
      #f)))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
      (set-cdr! record value)
      (set-cdr! table
                (cons (cons key value) (cdr table)))))
  'ok)

(define (make-table)
  (list '*table*))
(define sample-table (make-table))

(define (main args)
  (print (insert! 'd 4 sample-table))
  (print (lookup 'd sample-table)))
