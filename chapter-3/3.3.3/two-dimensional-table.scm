(define (lookup key-1 key-2 table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
      (let ((record (assoc key-2 (cdr subtable))))
        (if record
          (cdr record)
          #f))
      #f)))

(define sample-table
  '(*table* (math (+ . 43) (- . 45) (* . 42))
            (letters (a . 97) (b . 98))))

(define (main args)
  (print (lookup 'math '* sample-table)))
