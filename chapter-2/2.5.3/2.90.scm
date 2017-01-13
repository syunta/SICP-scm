(load "./2.89")

(define (install-sparse-term-list-package)
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
            (let ((t1 (first-term L1)) (t2 (first-term L2)))
              (cond ((> (order t1) (order t2))
                     (adjoin-term
                       t1 (add-terms (rest-terms L1) L2)))
                    ((< (order t1) (order t2))
                     (adjoin-term
                       t2 (add-terms L1 (rest-terms L2))))
                    (else
                      (adjoin-term
                        (make-term (order t1)
                                   (add (coeff t1) (coeff t2)))
                        (add-terms (rest-terms L1)
                                   (rest-terms L2)))))))))

  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
      (the-empty-termlist)
      (add-terms (mul-term-by-all-terms (first-term L1) L2)
                 (mul-terms (rest-terms L1) L2))))

  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
      (the-empty-termlist)
      (let ((t2 (first-term L)))
        (adjoin-term
          (make-term (+ (order t1) (order t2))
                     (mul (coeff t1) (coeff t2)))
          (mul-term-by-all-terms t1 (rest-terms L))))))

  (define (=zero-terms? ts)
    (if (empty-termlist? ts)
      #t
      (and (=zero? (coeff (first-term ts)))
           (=zero-terms? (rest-terms ts)))))

  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
      term-list
      (cons term term-list)))

  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  (define (tag x) (attach-tag 'sparse x))
  (put 'add '(sparse sparse)
       (lambda (ts1 ts2) (tag (add-terms ts1 ts2))))
  (put 'mul '(sparse sparse)
       (lambda (ts1 ts2) (tag (mul-terms ts1 ts2))))
  (put '=zero? '(sparse)
       (lambda (ts) (tag (=zero-terms? ts))))
  'done)

(define (install-dense-term-list-package)
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
            (let ((t1 (first-term L1)) (t2 (first-term L2)))
              (cond ((> (order t1) (order t2))
                     (adjoin-term
                       t1 (add-terms (rest-terms L1) L2)))
                    ((< (order t1) (order t2))
                     (adjoin-term
                       t2 (add-terms L1 (rest-terms L2))))
                    (else
                      (adjoin-term
                        (make-term (order t1)
                                   (add (coeff t1) (coeff t2)))
                        (add-terms (rest-terms L1)
                                   (rest-terms L2)))))))))

  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
      (the-empty-termlist)
      (add-terms (mul-term-by-all-terms (first-term L1) L2)
                 (mul-terms (rest-terms L1) L2))))

  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
      (the-empty-termlist)
      (let ((t2 (first-term L)))
        (adjoin-term
          (make-term (+ (order t1) (order t2))
                     (mul (coeff t1) (coeff t2)))
          (mul-term-by-all-terms t1 (rest-terms L))))))

  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
      term-list
      (let ((n (- (order term) (- (length term-list) 1) 1)))
        (append (cons (coeff term)
                      (fill-zero n))
                term-list))))
  (define (fill-zero n)
    (if (zero? n)
      '()
      (cons 0 (fill-zero (- n 1)))))

  (define (=zero-terms? ts)
    (if (empty-termlist? ts)
      #t
      (and (=zero? (coeff (first-term ts)))
           (=zero-terms? (rest-terms ts)))))

  (define (the-empty-termlist) '())
  (define (first-term term-list)
    (let ((order (- (length term-list) 1)))
      (make-term order (car term-list))))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  (define (tag x) (attach-tag 'dense x))
  (put 'add '(dense dense)
       (lambda (ts1 ts2) (tag (add-terms ts1 ts2))))
  (put 'mul '(dense dense)
       (lambda (ts1 ts2) (tag (mul-terms ts1 ts2))))
  (put '=zero? '(dense)
       (lambda (ts) (tag (=zero-terms? ts))))
  'done)

(define (install-polynomial-package)
  ;; 内部手続き
  ;; 多項式型の表現
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  ;; 項と項リストの表現
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add (term-list p1)
                      (term-list p2)))
      (error "Polys not in same var -- ADD-POLY"
             (list p1 p2))))

  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (mul (term-list p1)
                      (term-list p2)))
      (error "Polys not in same var -- MUL-POLY"
             (list p1 p2))))

  (define (=zero-poly? p)
    (=zero? (term-list p)))

  ;; システムの他の部分とのインターフェース
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put '=zero? '(polynomial)
       (lambda (p) (=zero-poly? p)))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'done)

(define (main args)
  (install-polar-package)
  (install-rectangular-package)
  (install-scheme-number-package)
  (install-rational-package)
  (install-complex-package)
  (install-sparse-term-list-package)
  (install-dense-term-list-package)
  (install-polynomial-package)

  ;make-sparse-term-list などは省略

  (print (add (make-polynomial 'x '(dense 1 2 0 3 -2 -5))
              (make-polynomial 'x '(dense 2 3 4 1  1  0))))
  ;=> (polynomial x dense 3 5 4 4 -1 -5)
  (print (mul (make-polynomial 'x '(dense 1 1 1))
              (make-polynomial 'x '(dense 1 1 1))))
  ;=> (polynomial x dense 1 2 3 2 1)
  (print (add (make-polynomial 'x '(sparse (100 1) (2 2) (0 1)))
              (make-polynomial 'x '(sparse (100 1) (2 2) (0 1)))))
  ;=> (polynomial x sparse (100 2) (2 4) (0 2))
  )
