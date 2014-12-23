; table construction design
;
; (define *table* (make-table))
; (insert! '(2 9) 18 *table*)
; (insert! '(2 5) 10 *table*)
; (insert! '(5) 5 *table*)
;
;*table*
;   |
;   V
; (* *)->(* *)---------------------------->(* /)
;  |      |                                 |
;  V      V                                 V
; #f    (* *)                             (* *)
;        | |                               | | 
;        V V                               V V 
;        2 (* *)->(* *)------->(* /)       5 (* /)  
;           |      |            |             |   
;           V      V            V             V   
;          #f    (* *)        (* *)           5   
;                 | |          | |   
;                 V V          V V   
;                 5 (* /)      9 (* /)    
;                    |            | 
;                    V            V 
;                   10           18 

(define (make-table)
  (list #f))

(define (lookup keys table)
  (if (null? keys)
    (car table)
    (let ((record (assoc (car keys) (cdr table))))
      (if record
        (lookup (cdr keys) (cdr record))
        #f))))

(define (assoc key records)
  (cond ((null? records) #f)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (insert! keys value table)
  (if (null? keys)
    (begin (set-car! table value)
           'ok)
    (let* ((records (cdr table))
           (record (assoc (car keys) records)))
      (if record
        (insert! (cdr keys) value (cdr record))
        (let* ((new-record (cons (car keys) (make-table)))
               (new-records (cons new-record records)))
          (set-cdr! table new-records)
          (let ((new-insert-point (assoc (car keys) (cdr table))))
            (insert! (cdr keys)
                     value
                     (cdr new-insert-point))))))))

(define *table* (make-table))
(print (insert! '(2 5) 10 *table*))
(print (insert! '(2 10) 20 *table*))
(print (lookup '(2 5) *table*))
;=> 10
(print (lookup '(2 10) *table*))
;=> 20
(print (lookup '(2) *table*))
;=> #f
(print (insert! '(2) 2 *table*))
(print (lookup '(2) *table*))
;=> 2
(print (insert! '(2 5 6 10) 600 *table*))
(print (lookup '(2 5 6 10) *table*))
;=> 600
