(load "./2.65")

(define personnel-db
  (list->tree
    '((1  (name a) (age 26))
      (2  (name b) (age 24))
      (3  (name c) (age 28))
      (5  (name d) (age 26))
      (8  (name e) (age 26))
      (11 (name f) (age 23)))))

(define personnel-db-1
  '((1  (name a) (age 26))
    (2  (name b) (age 24))
    (3  (name c) (age 28))
    (5  (name d) (age 26))
    (8  (name e) (age 26))
    (11 (name f) (age 23))))

(define (key record)
  (car record))

(define (contents record)
  (cdr record))

(define (lookup-1 given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((equal? given-key (key (car set-of-records)))
         (car set-of-records))
        (else (lookup-1 given-key (cdr set-of-records)))))

(define (lookup given-key db)
  (if (null? db)
    'not-found
    (let ((record (entry db)))
      (cond ((= given-key (key record))
             (contents record))
            ((< given-key (key record))
             (lookup given-key (left-branch db)))
            ((> given-key (key record))
             (lookup given-key (right-branch db)))))))

(define (main args)
  (print (lookup-1 5 personnel-db-1))
  ;=> (5 (name d) (age 26))
  (print (lookup 5 personnel-db))
  ;=> ((name d) (age 26))
  )
