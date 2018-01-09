(define true #t)
(define false #f)

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
    set
    (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)        
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (cond ((null? set2) set1)
        ((null? set1) set2)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else (union-set (cdr set1)
                         (cons (car set1) set2)))))

(define (union-set-2 set1 set2)
  (define (iter set1 set2 result)
    (cond ((and (null? set1) (null? set2)) result)
          ((null? set1) (iter set2 '() result))
          (else (iter (cdr set1)
                      set2
                      (adjoin-set (car set1) result)))))
  (iter set1 set2 '()))

(define (unique-set seq)
  (if (null? seq)
    '()
    (adjoin-set (car seq)
                (unique-set (cdr seq)))))

(define (union-set-3 set1 set2)
  (unique-set (append set1 set2)))

(define (main args)
  (print (union-set '(4 1 3) '(6 4 5 3 1)))
  ;=> (6 4 5 3 1)
  (print (union-set-2 '(4 1 1 3) '(6 4 5 3 1 1)))
  ;=> (5 6 3 1 4)
  (print (union-set-3 '(4 1 1 3) '(6 4 5 3 1 1)))
  ;=> (6 4 5 3 1)
  )
