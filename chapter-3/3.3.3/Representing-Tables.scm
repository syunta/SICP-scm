(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
      (cdr record)
      #f)))

(define sample-table
  '(*table* (a . 1) (b . 2) (c . 3)))

(print (lookup 'd sample-table))
