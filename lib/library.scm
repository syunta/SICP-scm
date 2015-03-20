;; procedures have initial is 'A'
(define (Ack x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (Ack (- x 1)
                   (Ack x (- y 1))))))

(define (accumulate op initial seq)
  (if (null? seq)
    initial
    (op (car seq)
        (accumulate op initial (cdr seq)))))

(define (adjacent? x y seq)
  (cond
    ((or (null? seq) (null? (cdr seq))) #f)
    ((and (equal? x (car seq)) (equal? y (cadr seq))) #t)
    (else (adjacent? x y (cdr seq)))))

(define (any pred seq)
  (if (null? seq)
    #f
    (or (pred (car seq))
        (any pred (cdr seq)))))

(define (average . args)
  (/ (fold + 0.0 args) (length args)))

;; procedures have initial is 'B'
(define (butlast seq)
  (if (null? (cdr seq))
    nil
    (cons (car seq) (butlast (cdr seq)))))

(define (butlast-n seq n)
  (let ((len (length seq)))
    (let ((m (if (> n len)
               len
               n)))
      (take seq (- len m)))))

(define (before? x y seq)
  ((lambda (after-x)
     (if after-x
       (member y (cdr after-x))
       #f))
   (member x seq)))

;; procedures have initial is 'C'
(define (cube x)
  (* x x x))

(define (count x seq)
  (define (count-x x seq n)
    (cond
      ((null? seq) n)
      ((equal? x (car seq)) (count-x x (cdr seq) (+ n 1)))
      (else (count-x x (cdr seq) n))))
  (count-x x seq 0))

(define (count-leaf seq)
  (cond
    ((null? seq) 0)
    ((not (pair? seq)) 1)
    (else (+ (count-leaf (car seq)) (count-leaf (cdr seq))))))

(define (comb-num n r)
  (if (or (zero? r) (equal? r n))
    1
    (* (comb-num n (- r 1)) (/ (+ (- n r) 1) r))))

(define (combination n seq)
  (cond
    ((zero? n) (list nil))
    ((null? seq) (list nil))
    (else (flatmap (lambda (x)
                     (if (< (length (cdr (member x seq))) (- n 1))
                       nil
                       (map
                         (lambda (y) (cons x y))
                         (combination (- n 1) (cdr (member x seq))))))
                   seq))))

;; procedures have initial is 'D'
(define (double? seq)
  (and (pair? seq) (single? (cdr seq))))

(define (drop seq n)
  (if (or (= n 0) (null? seq))
    seq
    (drop (cdr seq) (- n 1))))

(define (difference x y)
  (cond
    ((null? x) nil)
    ((member (car x) y) (difference (cdr x) y))
    (else (cons (car x) (difference (cdr x) y)))))

(define (decode seq)
  (define (serialize-iter x n seq)
    (if (zero? n)
      seq
      (serialize-iter x (- n 1) (cons x seq))))
  (define (serialize pair)
    (serialize-iter (car pair) (cdr pair) nil))
  (flatmap serialize seq))

;; procedures have initial is 'E'
(define (enumerate-interval low high)
  (if (> low high)
    nil
    (cons low (enumerate-interval (+ low 1) high))))

(define (expand-num-list seq)
  (define (serialize-iter x end seq)
    (if (equal? x end)
      seq
      (serialize-iter (+ x 1) end (cons (+ x 1) seq))))
  (define (serialize pair)
    (serialize-iter (car pair) (cdr pair) (list (car pair))))
  (define (enl-iter seq expanded)
    (cond
      ((null? seq) (reverse expanded))
      ((pair? (car seq))
       (enl-iter (cdr seq) (append (serialize (car seq)) expanded)))
      (else (enl-iter (cdr seq) (cons (car seq) expanded)))))
  (enl-iter seq nil))

(define (encode seq)
  (map (lambda (xs) (cons (car xs) (length xs))) (pack seq)))

(define (every pred seq)
  (if (null? seq)
    #t
    (and (pred (car seq))
         (every pred (cdr seq)))))

;; procedures have initial is 'F'
(define (factorial n)
  (if (= n 1)
    1
    (* n (factorial (- n 1)))))

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define (fib-iter n)
  (fib-proc 1 0 n))

(define (fib-proc a b count)
  (if (= count 0)
    b
    (fib-proc (+ a b) a (- count 1))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (flat-tree seq)
  (cond
    ((null? seq) nil)
    ((not (pair? seq)) (list seq))
    (else (append
            (flat-tree (car seq))
            (flat-tree (cdr seq))))))

(define (for-each-list proc op initial seq)
  (if (null? seq)
    initial
    (op (proc (car seq)) (for-each-list proc op initial (cdr seq)))))

;; procedures have initial is 'G'
(define (group seq n)
  (if (null? seq)
    seq
    (cons (take seq n) (group (drop seq n) n))))

;; procedures have initial is 'I'
(define (intersection x y)
  (cond
    ((null? x) nil)
    ((member (car x) y) (cons (car x) (intersection (cdr x) y)))
    (else (intersection (cdr x) y))))

;; procedures have initial is 'L'
(define (longer? seq-x seq-y)
  (if (pair? seq-x)
    (if (pair? seq-y)
      (longer? (cdr seq-x) (cdr seq-y))
      #t)
    #f))

(define (last seq)
  (if (null? (cdr seq))
    seq
    (last (cdr seq))))

;; procedures have initial is 'M'
(define (max-list seq)
  (define (max-element seq mx)
    (cond
      ((null? seq) mx)
      ((> (car seq) mx)
       (max-element (cdr seq) (car seq)))
      (else (max-element (cdr seq) mx))))
  (if (null? seq)
    seq
    (max-element seq (car seq))))

(define (min-list seq)
  (define (min-element seq mn)
    (cond
      ((null? seq) mn)
      ((< (car seq) mn)
       (min-element (cdr seq) (car seq)))
      (else (min-element (cdr seq) mn))))
  (if (null? seq)
    seq
    (min-element seq (car seq))))

(define (merge-list op xs ys)
  (cond
    ((null? xs) ys)
    ((null? ys) xs)
    ((op (car xs) (car ys))
     (cons (car xs) (merge-list op (cdr xs) ys)))
    (else (cons (car ys) (merge-list op xs (cdr ys))))))

(define (merge-sort op seq)
  (define (merge-iter op seq)
    (cond
      ((null? seq) nil)
      ((null? (cdr seq)) seq)
      (else (merge-iter op
                        (cons (merge-list op (car seq) (cadr seq))
                              (merge-iter op (cddr seq)))))))
  (define (split seq)
    (group seq 1))
  (car (merge-iter op (split seq))))

(define (member-tree x seq)
  (define (flat-tree seq)
    (cond
      ((null? seq) nil)
      ((not (pair? seq)) (list seq))
      (else (append
              (flat-tree (car seq))
              (flat-tree (cdr seq))))))
  (if (member x (flat-tree seq)) #t #f))

(define (maplist proc seq)
  (define (maplist-iter proc seq mapped)
    (if (null? seq)
      (reverse mapped)
      (maplist-iter proc (cdr seq) (cons (proc seq) mapped))))
  (maplist-iter proc seq nil))

(define (my-equal? a b)
  (cond
    ((eq? a b) #t)
    ((or (not (pair? a)) (not (pair? b))) #f)
    (else (and (my-equal? (car a) (car b))
               (my-equal? (cdr a) (cdr b))))))

;; procedures have initial is 'N'
(define nil '())

;; procedures have initial is 'P'
(define (prime? n)
  (define (divides? x y)
    (= (remainder y x) 0))
  (define (find-divisor n test-divisor)
    (cond
      ((> (square test-divisor) n) n)
      ((divides? test-divisor n) test-divisor)
      (else (find-divisor n (+ test-divisor 1)))))
  (define (smallest-divisor n)
    (find-divisor n 2))
  (= n (smallest-divisor n)))

(define (position x seq)
  (define (count-pos x seq n)
    (cond
      ((null? seq) #f)
      ((equal? x (car seq)) n)
      (else (count-pos x (cdr seq) (+ n 1)))))
  (count-pos x seq 0))

(define (prefix xs ys)
  (cond
    ((null? ys) #t)
    ((equal? (car xs) (car ys)) (prefix (cdr xs) (cdr ys)))
    (else #f)))

(define (permutation n seq)
  (cond
    ((zero? n) (list nil))
    ((null? seq) (list nil))
    (else (flatmap
            (lambda (x)
              (map (lambda (y)
                     (cons x y))
                   (permutation (- n 1) (difference seq (list x)))))
            seq))))

(define (partition seq)
  (define (partition-rec seq n odds evens)
    (cond
      ((null? seq)
       (values (reverse odds) (reverse evens)))
      ((equal? 0 (remainder n 2))
       (partition-rec (cdr seq) (+ n 1) (cons (car seq) odds) evens))
      (else
        (partition-rec (cdr seq) (+ n 1) odds (cons (car seq) evens)))))
  (partition-rec seq 0 nil nil))

(define (pack seq)
  (define (pack-iter seq x xs packed)
    (cond
      ((null? seq) (reverse (cons xs packed)))
      ((equal? x (car seq))
       (pack-iter (cdr seq) x (cons x xs) packed))
      (else (pack-iter seq (car seq) nil (cons xs packed)))))
  (pack-iter seq (car seq) nil nil))

(define (pack-num-list seq)
  (define (pnl-iter seq x top packed)
    (cond
      ((null? seq) (if (equal? x top)
                     (reverse (cons top packed))
                     (reverse (cons (cons top x) packed))))
      ((equal? (+ x 1) (car seq))
       (pnl-iter (cdr seq) (car seq) top packed))
      ((equal? x top)
       (pnl-iter (cdr seq) (car seq) (car seq) (cons top packed)))
      (else
        (pnl-iter (cdr seq) (car seq) (car seq) (cons (cons top x) packed)))))
  (pnl-iter (cdr seq) (car seq) (car seq) nil))

;; procedures have initial is 'R'
(define (repeat-perm n seq)
  (cond
    ((zero? n) (list nil))
    ((null? seq) (list nil))
    (else (flatmap
            (lambda (x)
              (map (lambda (y)
                     (cons x y))
                   (repeat-perm (- n 1) seq)))
            seq))))

(define (repeat-comb n seq)
  (cond
    ((zero? n) (list nil))
    ((null? seq) (list nil))
    (else (flatmap
            (lambda (x)
              (map (lambda (y)
                     (cons x y))
                   (repeat-comb (- n 1) (member x seq))))
            seq))))

; procedures have initial is 'S'
(define (square x)
  (* x x))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (single? seq)
  (and (pair? seq) (null? (cdr seq))))

(define (subseq seq n m)
  (take (drop seq n) (- m n)))

(define (sum-list seq)
  (define (current-sum seq sum)
    (if (null? seq)
      sum
      (current-sum (cdr seq) (+ sum (car seq)))))
  (current-sum seq 0))

(define (set-of-list seq)
  (cond
    ((null? seq) seq)
    ((member (car seq) (cdr seq)) (set-of-list (cdr seq)))
    (else (cons (car seq) (set-of-list (cdr seq))))))

(define (suffix xs ys)
  (prefix (reverse xs) (reverse ys)))

(define (sublist xs ys)
  (cond
    ((null? xs) #f)
    ((or (null? ys) (prefix xs ys)) #t)
    (else (sublist (cdr xs) ys))))

(define (subst x y seq)
  (cond
    ((null? seq) nil)
    ((not (pair? seq))
     (if (equal? x seq)
       y
       seq))
    (else (cons (subst x y (car seq)) (subst x y (cdr seq))))))

(define (split-nth seq n)
  (define (split seq n front)
    (cond
      ((or (null? seq) (zero? n)) (values (reverse front) seq))
      (else (split (cdr seq) (- n 1) (cons (car seq) front)))))
  (split seq n nil))

(define (split-find x seq)
  (define (split-rec x seq front)
    (cond
      ((or (null? seq) (equal? x (car seq)))
       (values (reverse front) seq))
      (else (split-rec x (cdr seq) (cons (car seq) front)))))
  (split-rec x seq nil))

(define (split-ge x seq)
  (define (split-rec seq x low high)
    (cond
      ((null? seq) (values (reverse low) (reverse high)))
      ((>= x (car seq))
       (split-rec (cdr seq) x (cons (car seq) low) high))
      (else
        (split-rec (cdr seq) x low (cons (car seq) high)))))
  (split-rec seq x nil nil))

(define (sieve n)
  (define (sieve-iter x primes)
    (cond
      ((> x n) (reverse primes))
      ((prime? x) (sieve-iter (+ x 1) (cons x primes)))
      (else (sieve-iter (+ x 1) primes))))
  (sieve-iter 2 nil))

;; procedures have initial is 'T'
(define (take seq n)
  (if (or (= n 0) (not (pair? seq)))
    nil
    (cons (car seq) (take (cdr seq) (- n 1)))))

;; procedures have initial is 'U'
(define (union x y)
  (cond
    ((null? x) y)
    ((member (car x) y) (union (cdr x) y))
    (else (cons (car x) (union (cdr x) y)))))

;; local table package
(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
          (let ((record (assoc key-2 (cdr subtable))))
            (if record
              (cdr record)
              #f))
          #f)))
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
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))
