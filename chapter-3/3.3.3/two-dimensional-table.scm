(define (lookup key-1 key-2 table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
      (let ((record (assoc key-2 (cdr subtable))))
        (if record
          (cdr record)
          #f))
      #f)))

(define (insert! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
      (let ((record (assoc key-2 (cdr subtable))))
        (if record
          (set-cdr! record value)
          (set-cdr! subtable
                    (cons (cons key-2 value)
                          (cdr subtable)))))
      (set-cdr! table
                (cons (list key-1
                            (cons key-2 value))
                      (cdr table)))))
  'ok)


(define sample-table
  '(*table* (math (+ . 43) (- . 45) (* . 42))
            (letters (a . 97) (b . 98))))

(define (main args)
  (print (insert! 'letters 'a 99 sample-table))
  (print (insert! 'math '= 80 sample-table))
  (print (insert! 'char "俺" 26 sample-table))
  (print (lookup 'letters 'a sample-table))
  (print (lookup 'math '= sample-table))
  (print (lookup 'char "俺" sample-table)))
