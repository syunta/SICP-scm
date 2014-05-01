(define (element-of-set? x set)
  (define (test x set n)
    (cond ((null? set) #f)
          ((equal? x (car set)) #t)
          (else (test x (cdr set) (counter)))))
  (test x set (counter)))

(define (adjoin-set x set)
  (cons x set))

(define (intersection-set set1 set2)
  (define (test set1 set2 n)
    (cond ((or (null? set1) (null? set2)) '())
          ((element-of-set? (car set1) set2)
           (cons (car set1)
                 (test (cdr set1) set2 (counter))))
          (else (test (cdr set1) set2 (counter)))))
  (test set1 set2 (counter)))

(define (union-set set1 set2)
  (define (test set1 set2 n)
    (cond ((null? set2) set1)
          ((null? set1) set2)
          ((element-of-set? (car set1) set2)
           (test (cdr set1) set2 (counter)))
          (else (test (cdr set1) (cons (car set1) set2) (counter)))))
  (test set1 set2 (counter)))

[define (count-generator)
  (let ((count 0))
    (lambda () (set! count (+ count 1))))]

(define counter (count-generator))

(print (union-set '(1 4 8) '(1 2 5 6 7)))
(print (- (counter) 1))
