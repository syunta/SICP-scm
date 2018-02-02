; table construction design: (prev-node-value key left right next)
;
; (define *table* (make-table))
; (insert! '(4 5) 20 *table*)
; (insert! '(2 8) 16 *table*)
; (insert! '(5) 5 *table*)
;
;*table*
;   |
;   V
; (* *)->(* *)->(* *)->(* *)->(* /)
;  |      |      |      |      |
;  V      V      |      |      V
; #f      4      |      |    (* *)->(* *)->(* *)->(* *)->(* /)
;                |      |     |      |      |      |      |
;                |      |     V      V      V      V      V
;                |      |    #f      5    (* /)  (* /)  (* /)
;                |      |                  |      |      |
;                |      |                  V      V      V
;                |      |                 #f     #f     20
;                |      V
;                |    (* *)->(* *)->(* *)->(* *)->(* /)
;                |     |      |      |      |      |
;                |     V      V      V      V      V
;                |    #f      5    (* /)  (* /)  (* /)
;                |                  |      |      |
;                |                  V      V      V
;                |                 #f     #f      5
;                V
;              (* *)->(* *)->(* *)->(* *)->(* /)
;               |      |      |      |      |
;               V      V      V      V      V
;              #f      2    (* /)  (* /)  (* *)->(* *)->(* *)->(* *)->(* /)
;                            |      |      |      |      |      |      |
;                            V      V      V      V      V      V      V
;                           #f     #f     #f      8    (* /)  (* /)  (* /)
;                                                       |      |      |
;                                                       V      V      V
;                                                      #f     #f     16
(define (make-table) (list #f))
(define (make-node) (list #f (make-table) (make-table) (make-table)))

(define (entry record) (car record))
(define (key record) (cadr record))
(define (left record) (caddr record))
(define (right record) (cadddr record))
(define (next record) (car (cddddr record)))

(define (set-entry! record value) (set-car! record value))
(define (set-key! record key) (set-car! (cdr record) key))

(define (empty? table) (null? (cdr table)))

(define (lookup keys table)
  (if (null? keys)
    (entry table)
    (let ((record (lookup-record (car keys) table)))
      (cond ((empty? record) #f)
            (else (lookup (cdr keys) (next record)))))))

(define (lookup-record given-key table)
  (cond ((empty? table) table)
        ((= given-key (key table)) table)
        ((< given-key (key table))
         (lookup-record given-key (left table)))
        ((> given-key (key table))
         (lookup-record given-key (right table)))))

(define (insert! keys value table)
  (if (null? keys)
    (set-entry! table value)
    (let ((record (lookup-record (car keys) table)))
      (cond ((empty? record)
             (set-cdr! record (make-node))
             (set-key! record (car keys))
             (insert! (cdr keys) value (next record)))
            (else
              (insert! (cdr keys) value (next record)))))))

(define *table* (make-table))
(insert! '(4 5) 20 *table*)
(insert! '(2 8) 16 *table*)
(insert! '(5) 5 *table*)
(print *table*)
;=> (#f 4 (#f 2 (#f) (#f) (#f 8 (#f) (#f) (16))) (#f 5 (#f) (#f) (5)) (#f 5 (#f) (#f) (20)))
(print (lookup '(2 8) *table*))
;=> 16
(print (lookup '(2 8 2) *table*))
;=> #f
(print (lookup '(4 5) *table*))
;=> 20
(print (lookup '(4 7) *table*))
;=> #f
(print (lookup '(3) *table*))
;=> #f
(print (lookup '(5) *table*))
;=> 5

(insert! '(9 9 9 9) (* 81 81) *table*)
(print (lookup '(9 9 9) *table*))
;=> #f
(print (lookup '(9 9 9 9) *table*))
;=> 6561
(insert! '(9 9 9) (* 81 9) *table*)
(print (lookup '(9 9 9) *table*))
;=> 729
(print (lookup '(9 9 9 9) *table*))
;=> 6561
